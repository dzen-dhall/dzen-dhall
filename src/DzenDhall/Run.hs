{-# LANGUAGE TemplateHaskell #-}
module DzenDhall.Run where

import           DzenDhall.App as App
import           DzenDhall.Arguments
import           DzenDhall.Config
import           DzenDhall.Data
import           DzenDhall.Event
import           DzenDhall.Extra
import           DzenDhall.Runtime
import qualified DzenDhall.Animation.Marquee as Marquee
import qualified DzenDhall.Animation.Slider as Slider
import qualified DzenDhall.Parser

import           Control.Arrow
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State
import           Data.Containers.ListUtils (nubOrd)
import qualified Data.HashMap.Strict as H
import           Data.IORef
import           Data.Maybe
import qualified Data.Text
import           Data.Text (Text)
import qualified Data.Text.IO
import           GHC.IO.Handle
import           Lens.Micro
import           Lens.Micro.Extras
import           Lens.Micro.TH
import           System.Exit (ExitCode(..), exitWith)
import qualified System.IO
import           System.Process


data StartupState
  = StartupState
  { _automataHandles :: AutomataHandles
  , _scopeLevel :: Int
  , _scopeName :: Text
  , _suBarSettings :: BarSettings
  }

makeLenses ''StartupState


-- | Parses 'BarSpec's. For each 'Configuration' spawns its own dzen binary.
useConfigurations :: App [Async ()]
useConfigurations = do
  runtime <- App.getRuntime
  forM (view rtConfigurations runtime) (App.mapApp async . go)
  where
    go :: Configuration -> App ()
    go cfg = do
      let barTokens = cfg ^. cfgBarTokens
      let eiBar     = DzenDhall.Parser.runBarParser barTokens

      case eiBar of
        Left err -> liftIO $ do
          putStrLn $ "Internal error when parsing BarSpec, debug info: " <> show barTokens
          putStrLn $ "Error: " <> show err
          putStrLn $ "Please report as bug."
          exitWith $ ExitFailure 3

        Right (bar :: BarSpec) -> do
          startDzenBinary cfg bar


-- | Starts dzen binary according to 'BarSettings'.
startDzenBinary
  :: Configuration
  -> BarSpec
  -> App ()
startDzenBinary cfg barSpec = do

  runtime <- App.getRuntime

  let dzenBinary  = runtime ^. rtDzenBinary
      barSettings = cfg ^. cfgBarSettings
      fontWidth   = fromMaybe 10 $ barSettings ^. bsFontWidth

      extraFlags  = barSettings ^. bsExtraFlags
      fontFlags   = maybe [] (\font -> ["-fn", font]) $ barSettings ^. bsFont
      flags       = fontFlags <> extraFlags

  (bar :: Bar, handles :: AutomataHandles) <- runInitialization barSettings barSpec

  void $ liftIO $ async $ launchEventListener (runtime ^. rtNamedPipe) handles

  (mb_stdin, mb_stdout, _, _) <- liftIO $ do
    createProcess $ (proc dzenBinary flags) { std_out = CreatePipe
                                            , std_in  = CreatePipe
                                            }

  case (mb_stdin, mb_stdout) of

    (Just stdin, Just stdout) -> do
      liftIO $ do
        hSetEncoding  stdin  System.IO.utf8
        hSetEncoding  stdout System.IO.utf8
        hSetBuffering stdin  LineBuffering
        hSetBuffering stdout LineBuffering

      forever $ do
        output <- renderAST <$> collectSources fontWidth bar
        liftIO $ do
          if runtime ^. rtArguments ^. stdoutFlag == ToStdout
            then Data.Text.IO.putStrLn output
            else Data.Text.IO.hPutStrLn stdin output
          threadDelay (cfg ^. cfgBarSettings ^. bsUpdateInterval)
        modifyRuntime $ rtFrameCounter +~ 1

    _ -> liftIO $ do
      putStrLn $ "Couldn't open IO handles for dzen binary " <> show dzenBinary

type Initialization = StateT StartupState App

runInitialization :: BarSettings -> BarSpec -> App (Bar, AutomataHandles)
runInitialization bs barSpec =
  second (^. automataHandles) <$> runStateT (initialize barSpec) initialState
  where
    initialState = StartupState mempty 0 "scope" bs

-- | During initialization, IORefs for source outputs and caches are created.
-- Also, new thread for each source is created. This thread then updates the outputs.
initialize
  :: BarSpec
  -> Initialization Bar
initialize (BarSource source@(Source{escapeMode}))
  = lift . liftIO $ do

  outputRef <- newIORef ""
  cacheRef  <- newIORef Nothing

  void $ async (mkThread source outputRef cacheRef)

  pure $ BarSource $
    SourceHandle
    outputRef
    cacheRef
    escapeMode

initialize (BarMarquee i p) =
  BarMarquee i <$> initialize p
initialize (BarSlider slider children) = do
  bs <- (^. suBarSettings) <$> get

  -- Convert delay of a slider from microseconds to frames.
  let updateInterval = bs ^. bsUpdateInterval
      slider' = slider & sliderDelay %~ (\x -> x * 1000 `div` positive updateInterval)

  BarSlider slider' <$> mapM initialize children

initialize (BarAutomaton stt stateMap) = do

  -- This binding determines what state to enter first
  let initialState = ""

  scope <- (^. scopeName) <$> get

  let addScope = (<> ("@" <> scope))

  -- Add current scope to all addresses in state transition table
  let stt' = STT
           . H.fromList
           . map (first (_1 %~ addScope))
           . H.toList
           . unSTT
           $ stt

  -- Create a reference to the current state
  stateRef :: IORef Text <- lift . liftIO $ newIORef initialState

  -- Initialize all children
  stateMap' :: H.HashMap Text Bar <- mapM initialize stateMap

  -- Create a reference to the current Bar (so that collectSources will not need to
  -- look up the correct Bar in stateMap').
  barRef :: IORef Bar <- lift . liftIO $
    newIORef $ fromMaybe mempty (H.lookup initialState stateMap')

  let subscription = [ AutomatonSubscription stt' stateMap' stateRef barRef ]

  -- Absolute slot addresses (incl. scope)
  let slots :: [Text] = nubOrd $ (^. _1) <$> H.keys (unSTT stt')

  -- Add subscription for each slot
  modify $ automataHandles %~
    (\handleMap -> foldr (\slot -> H.insertWith (++) slot subscription) handleMap slots)

  -- No need to keep state transition table - hence ()
  pure $ BarAutomaton () barRef

initialize (BarListener slot child) = do

  scope <- (^. scopeName) <$> get
  BarListener (slot <> "@" <> scope) <$> initialize child

initialize (BarScope child) = do
  counter <- lift getCounter
  oldScopeName <- (^. scopeName) <$> get
  modify $ scopeName <>~ ("-" <> showPack counter)
  child' <- initialize child
  modify $ scopeName .~ oldScopeName
  pure child'

initialize (BarProp prop p) =
  BarProp prop <$> initialize p
initialize (Bars ps) =
  Bars <$> mapM initialize ps
initialize (BarRaw text) =
  pure $ BarRaw text
initialize (BarText text) =
  pure $ BarText text


-- | Run source process either once or forever, depending on source settings.
mkThread
  :: Source
  -> IORef Text
  -> Cache
  -> IO ()
mkThread Source { command = [] } outputRef cacheRef = do
  let message = "dzen-dhall error: no command specified"
  writeIORef cacheRef $ Just message
  writeIORef outputRef message
mkThread
  Source { updateInterval
         , command = (binary : args)
         , stdin }
  outputRef
  cacheRef = do

  let sourceProcess =
        (proc binary args) { std_out = CreatePipe
                           , std_in  = CreatePipe
                           , std_err = CreatePipe
                           }

  case updateInterval of

    -- If update interval is specified, loop forever.
    Just interval -> do

      forever $ do
        runSourceProcess sourceProcess outputRef cacheRef stdin

        threadDelay interval

    -- If update interval is not specified, run the source once.
    Nothing -> do
      runSourceProcess (proc binary args) outputRef cacheRef stdin

-- | Creates a process, subscribes to its stdout handle and updates the output ref.
runSourceProcess :: CreateProcess -> IORef Text -> Cache -> Maybe Text -> IO ()
runSourceProcess cp outputRef cacheRef mbInput = do
  (mb_stdin_hdl, mb_stdout_hdl, mb_stderr_hdl, ph) <- createProcess cp

  case (mb_stdin_hdl, mb_stdout_hdl, mb_stderr_hdl) of
    (Just stdin, Just stdout, _) -> do
      hSetEncoding  stdin  System.IO.utf8
      hSetEncoding  stdout System.IO.utf8
      hSetBuffering stdin  LineBuffering
      hSetBuffering stdout LineBuffering

      -- If the input is specified, write it to the stdin handle
      whenJust mbInput $ \text -> do
        Data.Text.IO.hPutStrLn stdin text
        hClose stdin

      output <- Data.Text.IO.hGetContents stdout

      -- Drop cache
      writeIORef cacheRef Nothing
      writeIORef outputRef output

      void $ waitForProcess ph

    _ -> do
      putStrLn "dzen-dhall error: Couldn't open IO handle(s)"

-- | Reads outputs of 'SourceHandle's and puts it into the AST.
collectSources
  :: Int
  -> Bar
  -> App AST
collectSources _ (BarSource handle) = liftIO $ do
  let outputRef  = handle ^. shOutputRef
      cacheRef   = handle ^. shCacheRef
      escapeMode = handle ^. shEscapeMode

  cache <- readIORef cacheRef
  case cache of
    Just escaped ->
      pure $ ASTText escaped
    Nothing -> do
      raw <- readIORef outputRef
      let escaped = escape escapeMode raw
      writeIORef cacheRef (Just escaped)
      pure $ ASTText escaped

collectSources fontWidth (BarMarquee marquee p) = do

  ast          <- collectSources fontWidth p
  frameCounter <- view rtFrameCounter <$> App.getRuntime
  pure $ Marquee.run fontWidth marquee ast frameCounter

collectSources fontWidth (BarSlider slider ss) = do

  frameCounter <- view rtFrameCounter <$> App.getRuntime
  asts         <- mapM (collectSources fontWidth) ss
  pure $ Slider.run slider frameCounter asts

collectSources fontWidth (BarAutomaton () ref) = do

  bar          <- liftIO (readIORef ref)
  collectSources fontWidth bar

collectSources fontWidth (BarListener slot child) = do

  namedPipe    <- view rtNamedPipe <$> App.getRuntime
  ast          <- collectSources fontWidth child

  -- Wrap AST into 7 clickable areas, one for each mouse button
  pure $ foldr (attachClickHandler namedPipe) ast allButtons
    where
      attachClickHandler :: String -> Button -> AST -> AST
      attachClickHandler namedPipe button =
        let command =
              ( "echo event:"
             <> renderButton button
             <> ",slot:"
             <> slot
             <> " >> "
             -- TODO: escape it
             <> Data.Text.pack namedPipe
              )
        in Prop $ CA (ClickableArea button command)

collectSources fontWidth (BarScope child) = do
  collectSources fontWidth child

collectSources fontWidth (BarProp prop child)
  = Prop prop <$> collectSources fontWidth child
collectSources fontWidth (Bars ps)
  = mconcat <$> mapM (collectSources fontWidth) ps
collectSources _         (BarText text)
  = pure $ ASTText text
collectSources _         (BarRaw text)
  = pure $ ASTText text


escape
  :: EscapeMode
  -> Text
  -> Text
escape EscapeMode{joinLines, escapeMarkup} =
  (if escapeMarkup then Data.Text.replace "^" "^^" else id) .
  (Data.Text.replace "\n" $ if joinLines then " " else "")


renderAST :: AST -> Text
renderAST EmptyAST = ""
renderAST (ASTText text) = text
renderAST (ASTs a b) = renderAST a <> renderAST b
renderAST (Prop property ast) =
  let inner = renderAST ast in

    case property of
      BG color ->
        "^bg(" <> renderColor color <> ")" <> inner <> "^bg()"
      FG color ->
        "^fg(" <> renderColor color <> ")" <> inner <> "^fg()"
      IB ->
        "^ib(1)" <> inner <> "^ib(0)"
      CA ca ->
        "^ca(" <> renderButton (ca ^. caButton)  <> "," <> ca ^. caCommand <> ")" <> inner <> "^ca()"
      PA (AbsolutePosition {x, y}) ->
        "^pa(" <> showPack x <> ";" <> showPack y <> ")" <> inner <> "^pa()"
      P position ->
        open <> inner <> close
        where
          -- Compensate position shift if possible
          (open, close) =
            case position of
              XY (x, y)  ->
                ( "^p(" <> showPack x    <> ";" <> showPack y    <> ")"
                , "^p(" <> showPack (-x) <> ";" <> showPack (-y) <> ")"
                )
              P_RESET_Y  -> ("^p()", "")
              P_LOCK_X   -> ("^p(_LOCK_X)", "")
              P_UNLOCK_X -> ("^p(_UNLOCK_X)", "")
              P_LEFT     -> ("^p(_LOCK_X)^p(_LEFT)", "^p(_UNLOCK_X)")
              P_RIGHT    -> ("^p(_LOCK_X)^p(_RIGHT)", "^p(_UNLOCK_X)")
              P_TOP      -> ("^p(_TOP)", "^p()")
              P_CENTER   -> ("^p(_CENTER)", "^p()")
              P_BOTTOM   -> ("^p(_BOTTOM)", "^p()")

renderAST (Container shape width) =
  "^p(_LOCK_X)" <> shapeSpec <> "^p(_UNLOCK_X)^ib(1)" <> padding <> "^ib(0)"
  where
    padding = Data.Text.justifyRight width ' ' ""
    shapeSpec =
      case shape of
        I path -> "^i("  <> path       <> ")"
        R w h  -> "^r("  <> showPack w <> "x" <> showPack h <> ")"
        RO w h -> "^ro(" <> showPack w <> "x" <> showPack h <> ")"
        C r    -> "^c("  <> showPack r <> ")"
        CO r   -> "^co(" <> showPack r <> ")"
        Padding -> ""


allButtons :: [Button]
allButtons =
  [ MouseLeft
  , MouseMiddle
  , MouseRight
  , MouseScrollUp
  , MouseScrollDown
  , MouseScrollLeft
  , MouseScrollRight
  ]


renderButton :: Button -> Text
renderButton = \case
  MouseLeft -> "1"
  MouseMiddle -> "2"
  MouseRight -> "3"
  MouseScrollUp -> "4"
  MouseScrollDown -> "5"
  MouseScrollLeft -> "6"
  MouseScrollRight -> "7"


renderColor :: Color -> Text
renderColor = \case
  ColorHex r -> r
  ColorName r -> r
