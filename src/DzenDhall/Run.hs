{-# LANGUAGE NamedFieldPuns #-}
module DzenDhall.Run where

import Control.Concurrent
import Control.Monad
import Data.IORef
import qualified Data.Text
import Data.Text (Text)
import qualified Data.Text.IO
import DzenDhall.Config
import DzenDhall.Data
import System.Process
import GHC.IO.Handle
import qualified DzenDhall.Parser
import qualified Text.Parsec
import System.Exit (ExitCode(..), exitWith)
import DzenDhall.Runtime (Runtime(..))
import Control.Concurrent.Async
import DzenDhall.App as App


-- | During initialization, IORefs for source outputs and caches are created.
-- Also, new thread for each source is created. This thread then updates the outputs.
initialize :: Bar SourceSettings -> App (Bar SourceHandle)
initialize (Source settings@(SourceSettings{..})) = liftIO $ do
  outputRef <- newIORef ""
  cacheRef <- newIORef Nothing
  void $ async (mkThread settings outputRef cacheRef)
  let shEscapeMode = escapeMode
  pure $ Source (SourceHandle {..})

initialize (Txt text) = pure $ Txt text
initialize (Marquee i p) = Marquee i <$> initialize p
initialize (Color color p) = Color color <$> initialize p
initialize (Bars ps) = Bars <$> mapM initialize ps
initialize (Raw text) = pure $ Raw text

-- | Run source process either once or forever, depending on source settings.
mkThread :: SourceSettings -> IORef Text -> Cache -> IO ()
mkThread SourceSettings { command = [] } outputRef cacheRef = do
  let message = "dzen-dhall error: no command specified"
  writeIORef cacheRef $ Just message
  writeIORef outputRef message
mkThread SourceSettings { updateInterval, command = (binary : args), stdin } outputRef cacheRef = do

  let sourceProcess =
        (proc binary args) { std_out = CreatePipe
                           , std_in  = CreatePipe
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

-- | Produces an AST from 'Bar'.
collectSources :: Bar SourceHandle -> IO AST
collectSources (Source SourceHandle { outputRef, cacheRef, shEscapeMode })
  = do
  cache <- readIORef cacheRef
  case cache of
    Just escaped ->
      pure $ ASTText escaped
    Nothing -> do
      raw <- readIORef outputRef
      let escaped = escape shEscapeMode raw
      writeIORef cacheRef (Just escaped)
      pure $ ASTText escaped
collectSources (Txt text)
  = pure $ ASTText text
collectSources (Raw text)
  = pure $ ASTText text
collectSources (Marquee mqSettings p)
  = collectSources p -- TODO
collectSources (Color color p)
  = Prop (FG color) <$> collectSources p -- TODO
collectSources (Bars ps)
  = mconcat <$> mapM collectSources ps

escape :: EscapeMode -> Text -> Text
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

showPack :: Show a => a -> Text
showPack = Data.Text.pack . show

loopWhileM :: Monad m => m Bool -> m () -> m ()
loopWhileM pr act = do
    b <- pr
    when b $ do
      act
      loopWhileM pr act

whenJust :: (Monad m, Monoid b) => Maybe a -> (a -> m b) -> m b
whenJust = flip $ maybe (return mempty)

useConfigurations :: App [Async ()]
useConfigurations = do
  Runtime{rtConfigurations} <- App.getRuntime
  forM rtConfigurations (App.mapApp async . go)
  where
    go :: Configuration -> App ()
    go cfg@Configuration{bar} = do

      let eiBarSpec = Text.Parsec.runParser DzenDhall.Parser.bar () "BarSpec #1" bar

      case eiBarSpec of
        Left err -> liftIO $ do
          putStrLn $ "Internal error #1, debug info: " <> show bar
          putStrLn $ "Error: " <> show err
          exitWith (ExitFailure 3)

        Right (barSS :: Bar SourceSettings) -> do
          startDzenBinary cfg barSS

startDzenBinary :: Configuration -> Bar SourceSettings -> App ()
startDzenBinary
  Configuration{settings = BarSettings{bsExtraFlags, bsUpdateInterval}}
  barSS = do
  Runtime{rtDzenBinary} <- App.getRuntime

  barSH :: Bar SourceHandle <- initialize barSS

  (mb_stdin, mb_stdout, mb_stderr, _) <- liftIO $
    createProcess $ (proc rtDzenBinary bsExtraFlags) { std_out = CreatePipe
                                                     , std_in  = CreatePipe
                                                     }

  case (mb_stdin, mb_stdout, mb_stderr) of

    (Just stdin, Just stdout, _) -> liftIO $ do
      hSetBuffering stdin  LineBuffering
      hSetBuffering stdout LineBuffering

      forever $ do
        output <- renderAST <$> collectSources barSH
        Data.Text.IO.hPutStrLn stdin output
        threadDelay bsUpdateInterval

    _ -> liftIO $ do
      putStrLn $ "Couldn't open IO handles for dzen binary " <> show rtDzenBinary
