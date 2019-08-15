module DzenDhall.App.StartingUp where

import           DzenDhall.AST
import           DzenDhall.AST.Render
import           DzenDhall.App as App
import           DzenDhall.Arguments
import           DzenDhall.Config
import           DzenDhall.Data
import           DzenDhall.Event
import           DzenDhall.Extra
import           DzenDhall.Runtime
import qualified DzenDhall.Animation.Marquee as Marquee
import qualified DzenDhall.Animation.Slider as Slider

import           Control.Arrow
import           Control.Concurrent
import           Control.Exception hiding (handle)
import           Control.Monad
import           Data.Containers.ListUtils (nubOrd)
import           Data.IORef
import           Data.Maybe
import           Data.Text (Text)
import           GHC.IO.Handle
import           Lens.Micro
import           Lens.Micro.Extras
import           System.Directory
import           System.FilePath ((</>))
import           System.Posix.Files
import           System.Process
import           System.Random
import qualified Data.HashMap.Strict as H
import qualified Data.Text
import qualified Data.Text.IO
import qualified System.IO


startUp
  :: Configuration
  -> Bar Marshalled
  -> App StartingUp (Bar Initialized, AutomataHandles, BarRuntime)
startUp cfg bar = do
  bar' <- initialize bar
  startupState <- get
  let handles = startupState ^. ssAutomataHandles
  barRuntime <- mkBarRuntime cfg
  pure (bar', handles, barRuntime)


mkBarRuntime
  :: Configuration
  -> App StartingUp BarRuntime
mkBarRuntime cfg = do
  runtime <- getRuntime

  let dzenBinary  = runtime ^. rtDzenBinary
      barSettings = cfg ^. cfgBarSettings

      extraFlags  = barSettings ^. bsExtraFlags
      fontFlags   = maybe [] (\font -> ["-fn", font]) $ barSettings ^. bsFont
      flags       = fontFlags <> extraFlags

  tmpDir <- liftIO $
    getTemporaryDirectory `catch` \(_e :: IOException) -> getCurrentDirectory
  randomSuffix <- liftIO $
    take 10 . randomRs ('a','z') <$> newStdGen
  let namedPipe = tmpDir </> "dzen-dhall-rt-" <> randomSuffix

  liftIO $ createNamedPipe namedPipe (ownerReadMode `unionFileModes` ownerWriteMode)

  handle <- case runtime ^. rtArguments ^. stdoutFlag of

    ToStdout -> pure System.IO.stdout

    ToDzen -> do
      (mb_stdin, mb_stdout, _, _) <- liftIO $
        createProcess $
          (proc (runtime ^. rtDzenBinary) flags)
            { std_out = CreatePipe
            , std_in  = CreatePipe }

      case (mb_stdin, mb_stdout) of
        (Just stdin, Just stdout) -> liftIO $ do
          hSetEncoding  stdin  System.IO.utf8
          hSetEncoding  stdout System.IO.utf8
          hSetBuffering stdin  LineBuffering
          hSetBuffering stdout LineBuffering
          pure stdin
        _ -> App.exit 4 $
          "Couldn't open IO handles for dzen binary " <>
          showPack dzenBinary

  pure $ BarRuntime cfg 0 namedPipe handle


-- | During initialization, IORefs for source outputs and caches are created.
-- Also, new thread for each source is created. This thread then updates the outputs.
initialize
  :: Bar Marshalled
  -> App StartingUp (Bar Initialized)
initialize (BarSource source@Source{escapeMode})
  = liftIO $ do

  outputRef <- newIORef ""
  cacheRef  <- newIORef Nothing

  void $ forkIO (mkThread source outputRef cacheRef)

  pure $ BarSource $ SourceHandle outputRef cacheRef escapeMode

initialize (BarMarquee i p) =
  BarMarquee i <$> initialize p
initialize (BarSlider slider children) = do
  barSettings <- (^. ssBarSettings) <$> get

  -- Convert delay of a slider from microseconds to frames.
  let updateInterval = barSettings ^. bsUpdateInterval
      slider' = slider & sliderDelay %~ (\x -> x * 1000 `div` positive updateInterval)

  BarSlider slider' <$> mapM initialize children

initialize (BarAutomaton address stt stateMap) = do

  -- This binding determines what state to enter first
  let initialState = ""

  scope <- (^. ssScopeName) <$> get

  let addScope = (<> ("@" <> scope))

  -- Add current scope to all addresses in state transition table
  let stt' = STT
           . H.fromList
           . map (first (_1 %~ addScope))
           . H.toList
           . unSTT
           $ stt

  -- Create a reference to the current state
  stateRef :: IORef Text <- liftIO $ newIORef initialState

  -- Initialize all children
  stateMap' :: H.HashMap Text (Bar Initialized) <- mapM initialize stateMap

  -- Create a reference to the current Bar (so that collectSources will not need to
  -- look up the correct Bar in stateMap').
  barRef :: IORef (Bar Initialized) <- liftIO $
    newIORef $ fromMaybe mempty (H.lookup initialState stateMap')

  let subscription = [ AutomatonSubscription stt' stateMap' stateRef barRef ]

  -- Absolute slot addresses (incl. scope)
  let slots :: [Text] = nubOrd $ (^. _1) <$> H.keys (unSTT stt')

  -- Add subscription for each slot
  modify $ ssAutomataHandles %~
    (\handleMap -> foldr (\slot -> H.insertWith (++) slot subscription) handleMap slots)

  pure $ BarAutomaton address () barRef

initialize (BarListener slot child) = do

  scope <- (^. ssScopeName) <$> get
  BarListener (slot <> "@" <> scope) <$> initialize child

initialize (BarScope child) = do
  counter <- getCounter
  oldScopeName <- (^. ssScopeName) <$> get
  modify $ ssScopeName <>~ ("-" <> showPack counter)
  child' <- initialize child
  modify $ ssScopeName .~ oldScopeName
  pure child'

initialize (BarProp prop p) =
  BarProp prop <$> initialize p
initialize (BarPadding width padding p) =
  BarPadding width padding <$> initialize p
initialize (Bars ps) =
  Bars <$> mapM initialize ps
initialize (BarShape shape) =
  pure $ BarShape shape
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
         , input }
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
        runSourceProcess sourceProcess outputRef cacheRef input

        threadDelay interval

    -- If update interval is not specified, run the source once.
    Nothing -> do
      runSourceProcess (proc binary args) outputRef cacheRef input


-- | Creates a process, subscribes to its stdout handle and updates the output ref.
runSourceProcess
  :: CreateProcess
  -> IORef Text
  -> Cache
  -> Maybe Text
  -> IO ()
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


-- | Reads outputs of 'SourceHandle's and puts them into an AST.
collectSources
  :: Int
  -> Bar Initialized
  -> App Forked AST
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
  frameCounter <- view brFrameCounter <$> App.get
  pure $ Marquee.run fontWidth marquee ast frameCounter

collectSources fontWidth (BarSlider slider ss) = do

  frameCounter <- view brFrameCounter <$> App.get
  asts         <- mapM (collectSources fontWidth) ss
  pure $ Slider.run slider frameCounter asts

collectSources fontWidth (BarAutomaton _ _ ref) = do

  bar          <- liftIO (readIORef ref)
  collectSources fontWidth bar

collectSources fontWidth (BarListener slot child) = do

  namedPipe    <- view brNamedPipe <$> App.get
  ast          <- collectSources fontWidth child

  -- Wrap AST into 7 clickable areas, one for each mouse button
  pure $ foldr (attachClickHandler namedPipe) ast allButtons
    where
      attachClickHandler :: String -> Button -> AST -> AST
      attachClickHandler namedPipe button =
        let command =
              ( "echo event:"
             <> runRender button
             <> ",slot:"
             <> slot
             <> " >> "
             -- TODO: escape it
             <> Data.Text.pack namedPipe
              )
        in ASTProp $ CA (ClickableArea button command)

collectSources fontWidth (BarScope child) = do
  collectSources fontWidth child

collectSources fontWidth (BarPadding width padding child) = do
  ASTPadding width padding <$> collectSources fontWidth child

collectSources _         (BarShape shape) = do
  pure $ ASTShape shape

collectSources fontWidth (BarProp prop child)
  = ASTProp prop <$> collectSources fontWidth child
collectSources fontWidth (Bars ps)
  = mconcat <$> mapM (collectSources fontWidth) ps
collectSources _         (BarText text)
  = pure $ ASTText text
collectSources _         (BarRaw text)
  = pure $ ASTText text


-- | Escape @^@ characters and replace newlines.
escape
  :: EscapeMode
  -> Text
  -> Text
escape EscapeMode{joinLines, escapeMarkup} =
  (if escapeMarkup then Data.Text.replace "^" "^^" else id) .
  (Data.Text.replace "\n" $ if joinLines then " " else "")


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
