module DzenDhall.Run where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Monad
import           Data.IORef
import           Data.Maybe
import qualified Data.Text
import           Data.Text (Text)
import qualified Data.Text.IO
import qualified DzenDhall.Animation.Marquee as Marquee
import           DzenDhall.App as App
import           DzenDhall.Config
import           DzenDhall.Data
import           DzenDhall.Extra
import qualified DzenDhall.Parser
import           DzenDhall.Runtime
import           GHC.IO.Handle
import           Lens.Micro
import           Lens.Micro.Extras
import           System.Exit (ExitCode(..), exitWith)
import           System.Process
import qualified System.IO


-- | Parses 'BarSpec's. For each 'Configuration' spawns its own dzen binary.
useConfigurations :: App [Async ()]
useConfigurations = do
  runtime <- App.getRuntime
  forM (view rtConfigurations runtime) (App.mapApp async . go)
  where
    go :: Configuration -> App ()
    go cfg = do
      let barSpec = cfg ^. cfgBarSpec
      let eiBar   = DzenDhall.Parser.runBarParser barSpec

      case eiBar of
        Left err -> liftIO $ do
          putStrLn $ "Internal error when parsing BarSpec, debug info: " <> show barSpec
          putStrLn $ "Error: " <> show err
          putStrLn $ "Please report as bug."
          exitWith $ ExitFailure 3

        Right (bar :: Bar Source) -> do
          startDzenBinary cfg bar


-- | Starts dzen binary according to 'BarSettings'.
startDzenBinary
  :: Configuration
  -> Bar Source
  -> App ()
startDzenBinary cfg bar = do

  runtime <- App.getRuntime

  let dzenBinary  = runtime ^. rtDzenBinary
      barSettings = cfg ^. cfgBarSettings
      fontWidth   = fromMaybe 10 $ barSettings ^. bsFontWidth

      extraFlags  = barSettings ^. bsExtraFlags
      fontFlags   = maybe [] (\font -> ["-fn", font]) $ barSettings ^. bsFont
      flags       = fontFlags <> extraFlags

  bar' :: Bar SourceHandle <- initialize bar

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
        output <- renderAST <$> collectSources fontWidth bar'
        liftIO $ do
          Data.Text.IO.hPutStrLn stdin output
          threadDelay (cfg ^. cfgBarSettings ^. bsUpdateInterval)
        modifyRuntime $ rtFrameCounter +~ 1

    _ -> liftIO $ do
      putStrLn $ "Couldn't open IO handles for dzen binary " <> show dzenBinary


-- | During initialization, IORefs for source outputs and caches are created.
-- Also, new thread for each source is created. This thread then updates the outputs.
initialize :: Bar Source -> App (Bar SourceHandle)
initialize (BarSource settings@(Source{..})) = liftIO $ do

  outputRef <- newIORef ""
  cacheRef <- newIORef Nothing

  void $ async (mkThread settings outputRef cacheRef)

  pure $ BarSource $
    SourceHandle
    outputRef
    cacheRef
    escapeMode

initialize (BarMarquee i p) =
  BarMarquee i <$> initialize p
initialize (BarSlider slider children) =
  BarSlider slider <$> mapM initialize children
initialize (BarColor color p) =
  BarColor color <$> initialize p
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
  (mb_stdin_hdl, mb_stdout_hdl, mb_stderr_hdl, _) <- createProcess cp

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

    _ -> do
      putStrLn "dzen-dhall error: Couldn't open IO handle(s)"


-- | Reads outputs of 'SourceHandle's and puts it into the AST.
collectSources
  :: Int
  -> Bar SourceHandle
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
  asts <- mapM (collectSources fontWidth) ss
  pure mempty
collectSources fontWidth (BarColor color p)
  = Prop (FG color) <$> collectSources fontWidth p
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
        "^bg" <> color <> ")" <> inner <> "^bg()"
      FG color ->
        "^fg" <> color <> ")" <> inner <> "^fg()"
      IB ->
        "^ib(1)" <> inner <> "^ib(0)"
      CA (event, handler) ->
        "^ca(" <> event <> "," <> handler <> ")" <> inner <> "^ca()"
      P position -> positionSpec <> inner
        where
          positionSpec =
            case position of
              XY (x, y)  -> "^p(" <> showPack x <> ";" <> showPack y <> ")"
              ResetY     -> "^p()"
              P_LOCK_X   -> "^p(_LOCK_X)"
              P_UNLOCK_X -> "^p(_UNLOCK_X)"
              P_LEFT     -> "^p(_LEFT)"
              P_RIGHT    -> "^p(_RIGHT)"
              P_TOP      -> "^p(_TOP)"
              P_CENTER   -> "^p(_CENTER)"
              P_BOTTOM   -> "^p(_BOTTOM)"

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
