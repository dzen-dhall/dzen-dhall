module DzenDhall.App.StartingUp where

import           DzenDhall.AST
import           DzenDhall.App as App
import           DzenDhall.Arguments
import           DzenDhall.Config
import           DzenDhall.Data
import           DzenDhall.Extra
import           DzenDhall.Runtime.Data
import qualified DzenDhall.Animation.Marquee as Marquee
import qualified DzenDhall.Animation.Slider as Slider

import           Control.Arrow
import           Control.Applicative
import           Control.Concurrent
import           Control.Monad
import           Data.Containers.ListUtils (nubOrd)
import           Data.IORef
import           Data.Maybe
import           Data.Text (Text)
import           GHC.IO.Handle
import           Lens.Micro
import           Lens.Micro.Extras
import           System.Environment
import           System.Posix.Files
import           System.Process
import qualified Data.HashMap.Strict as H
import qualified Data.Text
import qualified Data.Text.IO
import qualified System.IO


startUp
  :: Configuration
  -> Bar Marshalled
  -> App StartingUp (Bar Initialized, AutomataHandles, BarRuntime, ClickableAreas)
startUp cfg bar = do
  barRuntime  <- mkBarRuntime cfg
  bar'        <- initialize bar

  state       <- get

  environment <- liftIO getEnvironment

  forM_ (state ^. ssSourceQueue) $
    \(source, outputRef, cacheRef, scope) -> do
      mkThread environment barRuntime source outputRef cacheRef scope

  forM_ (state ^. ssVariableDefinitions) $
    \(scope, name, value) -> do

      let fileName =
            state ^. ssVariableFilePrefix <>
            Data.Text.unpack scope <>
            "-v-" <>
            Data.Text.unpack name

      liftIO $ do
        Data.Text.IO.writeFile fileName value
        setFileMode fileName $
          ownerReadMode  `unionFileModes`
          groupReadMode  `unionFileModes`
          ownerWriteMode `unionFileModes`
          groupWriteMode


  pure (bar', state ^. ssAutomataHandles, barRuntime, state ^. ssClickableAreas)


mkBarRuntime
  :: Configuration
  -> App StartingUp BarRuntime
mkBarRuntime cfg = do
  runtime <- getRuntime
  state   <- get

  let dzenBinary  = runtime ^. rtDzenBinary
      barSettings = cfg ^. cfgBarSettings

      extraArgs  = barSettings ^. bsExtraArgs
      fontArgs   = maybe [] (\font -> ["-fn", font]) $ barSettings ^. bsFont
      args       = fontArgs <> extraArgs

      namedPipe          = state ^. ssNamedPipe
      emitterFile        = state ^. ssEmitterFile
      getterFile         = state ^. ssGetterFile
      setterFile         = state ^. ssSetterFile
      variableFilePrefix = state ^. ssVariableFilePrefix

  liftIO $
    createNamedPipe namedPipe (ownerReadMode `unionFileModes` ownerWriteMode)

  liftIO $ do
    forM_ [ ( [ "#!/usr/bin/env bash"
              , "SCOPE=\"$1\""
              , "SLOT=\"$2\""
              , "EVENT=\"$3\""
              , "echo event:\"$EVENT\",slot:\"$SLOT\"@\"$SCOPE\" >> " <>
                Data.Text.pack namedPipe
              ],
              emitterFile
            )

          , ( [ "#!/usr/bin/env bash"
              , "SCOPE=\"$1\""
              , "VAR=\"$2\""
              , "cat \"" <> Data.Text.pack variableFilePrefix <> "$SCOPE-v-$VAR\""
              ]
            , getterFile
            )

          , ( [ "#!/usr/bin/env bash"
              , "SCOPE=\"$1\""
              , "VAR=\"$2\""
              , "VALUE=\"$3\""
              , "echo \"$VALUE\" > " <>
                Data.Text.pack variableFilePrefix <> "\"$SCOPE-v-$VAR\""
              ]
            , setterFile
            )
          ] $

      \(codeLines, file) -> do

        Data.Text.IO.writeFile file $ fromLines codeLines
        setFileMode file $
          ownerExecuteMode `unionFileModes`
          groupExecuteMode `unionFileModes`
          ownerReadMode    `unionFileModes`
          groupReadMode

  handle <- case runtime ^. rtArguments ^. stdoutFlag of

    ToStdout -> pure System.IO.stdout

    ToDzen -> do
      (mb_stdin, mb_stdout, _, _) <- liftIO $
        createProcess $
          (proc (runtime ^. rtDzenBinary) args)
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

  pure $ BarRuntime cfg 0 namedPipe emitterFile getterFile setterFile handle


-- | During initialization, IORefs for source outputs and caches are created.
-- Also, new threads for each unique source is created. These threads then
-- update the outputs.
initialize
  :: Bar Marshalled
  -> App StartingUp (Bar Initialized)
initialize (BarSource source@Source{escapeMode}) = do

  state <- get

  -- We don't want to spawn separate threads for identical `Source`s within
  -- the same scope.
  -- A HashMap is used to cache all references to sources and prevent
  -- duplication when possible.
  -- Note that identical sources from distinct scopes are intentionally allowed:
  -- we don't want separate scopes to affect each other's behavior.

  let mbCached = H.lookup (state ^. ssScopeName, source) (state ^. ssSourceCache)

      createRefs :: App StartingUp (IORef Text, Cache)
      createRefs = do
        (outputRef, cacheRef) <- liftIO $
          liftA2 (,) (newIORef "") (newIORef Nothing)
        modify $ ssSourceQueue <>~
          [(source, outputRef, cacheRef, state ^. ssScopeName)]
        pure (outputRef, cacheRef)

  (outputRef, cacheRef) <- maybe createRefs pure mbCached

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
  state <- get

  let scope = state ^. ssScopeName

      mbCached = H.lookup (scope, address) (state ^. ssAutomataCache)

      newBarRef = do
        let initialState = ""

        -- Add current scope to state transition table
        let stt' = STT
                 . H.fromList
                 . map (first (_scope .~ state ^. ssScopeName))
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

        -- Add a subscription for each slot
        modify $ ssAutomataHandles %~
          (\handleMap -> foldr (
              \slot ->
                H.insertWith (++) (slot, scope) subscription
              )
            handleMap slots
          )

        -- Cache this automaton
        modify $ ssAutomataCache %~ H.insert (scope, address) barRef

        pure barRef

  barRef <- maybe newBarRef pure mbCached

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

initialize (BarProp (CA ca) child) = do

  identifier <- getCounter
  namedPipe <- get <&> (^. ssNamedPipe)

  let command =
        "echo click:" <> showPack identifier <> " >> " <> Data.Text.pack namedPipe

  modify $ ssClickableAreas %~ H.insert identifier (ca ^. caCommand)

  BarProp (CA (ca & caCommand .~ command)) <$> initialize child

initialize (BarProp prop p) =
  BarProp prop <$> initialize p
initialize (BarDefine var) = do

  scope <- get <&> (^. ssScopeName)

  modify $
    ssVariableDefinitions %~
    ((scope, var ^. varName, var ^. varValue) :)

  pure mempty
initialize (BarPad width padding p) =
  BarPad width padding <$> initialize p
initialize (BarTrim width direction p) =
  BarTrim width direction <$> initialize p
initialize (Bars ps) =
  Bars <$> mapM initialize ps
initialize (BarShape shape) =
  pure $ BarShape shape
initialize (BarMarkup text) =
  pure $ BarMarkup text
initialize (BarText text) =
  pure $ BarText text


-- | Run source process either once or forever, depending on source settings.
mkThread
  :: [(String, String)]
  -> BarRuntime
  -> Source
  -> IORef Text
  -> Cache
  -> Text
  -> App StartingUp ()
mkThread _ _ Source { command = [] } outputRef cacheRef _scope = do
  let message = "dzen-dhall error: no command specified"
  liftIO $ do
    writeIORef cacheRef $ Just message
    writeIORef outputRef message
mkThread
  environment
  barRuntime
  Source { updateInterval
         , command = (binary : args)
         , input }
  outputRef
  cacheRef
  scope = do

  let emitter =
        barRuntime ^. brEmitterScript <> " " <> Data.Text.unpack scope
      getter =
        barRuntime ^. brGetterScript  <> " " <> Data.Text.unpack scope
      setter =
        barRuntime ^. brSetterScript  <> " " <> Data.Text.unpack scope

      sourceProcess =
        (proc binary args) { std_out = CreatePipe
                           , std_in  = CreatePipe
                           , std_err = CreatePipe
                           , env = Just $
                             [ ("EMIT", emitter)
                             , ("GET",  getter)
                             , ("SET",  setter)
                             ] <>
                             environment
                           }

  void $ liftIO $ forkIO $ case updateInterval of

    -- If update interval is specified, loop forever.
    Just interval -> do

      forever $ do
        runSourceProcess sourceProcess outputRef cacheRef input

        threadDelay interval

    -- If update interval is not specified, run the source once.
    Nothing -> do
      runSourceProcess sourceProcess outputRef cacheRef input


-- | Creates a process, subscribes to its stdout handle and updates the output ref.
runSourceProcess
  :: CreateProcess
  -> IORef Text
  -> Cache
  -> Text
  -> IO ()
runSourceProcess cp outputRef cacheRef input = do
  (mb_stdin_hdl, mb_stdout_hdl, mb_stderr_hdl, ph) <- createProcess cp

  case (mb_stdin_hdl, mb_stdout_hdl, mb_stderr_hdl) of
    (Just stdin, Just stdout, _) -> do
      hSetEncoding  stdin  System.IO.utf8
      hSetEncoding  stdout System.IO.utf8
      hSetBuffering stdin  LineBuffering
      hSetBuffering stdout LineBuffering

      Data.Text.IO.hPutStrLn stdin input
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
             <> showPack button
             <> ",slot:"
             <> slot
             <> " >> "
             -- TODO: escape it
             <> Data.Text.pack namedPipe
              )
        in ASTProp $ CA (ClickableArea button command)

collectSources fontWidth (BarScope child) = do
  collectSources fontWidth child

collectSources fontWidth (BarPad width padding child) = do
  ASTPadding width padding <$> collectSources fontWidth child

collectSources fontWidth (BarTrim width direction child) = do
  ast <- collectSources fontWidth child
  case direction of
    DRight ->
      pure $ fst $ split width ast
    DLeft -> do
      let actualWidth = astWidth ast
      pure $ snd $ split (abs $ actualWidth - width) ast

collectSources _         (BarShape shape) = do
  pure $ ASTShape shape

collectSources fontWidth (BarProp prop child)
  = ASTProp prop <$> collectSources fontWidth child
collectSources _fontWidth (BarDefine _prop)
  = pure mempty
collectSources fontWidth (Bars ps)
  = mconcat <$> mapM (collectSources fontWidth) ps
collectSources _         (BarText text)
  = pure $ ASTText text
collectSources _         (BarMarkup text)
  = pure $ ASTText text


-- | Escape @^@ characters and replace newlines.
escape
  :: EscapeMode
  -> Text
  -> Text
escape EscapeMode{joinLines, escapeMarkup} =
  (if escapeMarkup
   then Data.Text.replace "^" "^^"
   else id) .
  (if joinLines
   then Data.Text.replace "\n" ""
   else fromMaybe "" . listToMaybe . Data.Text.lines)


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
