{-# LANGUAGE NamedFieldPuns #-}
module DzenDhall.Run where

import Control.Concurrent
import Control.Monad
import Data.IORef
import qualified Data.Text
import Data.Text (Text)
import Data.Text.IO
import DzenDhall.Config
import DzenDhall.Data
import System.Process
import GHC.IO.Handle

-- | During initialization, IORefs for source outputs and caches are created.
-- Also, new thread for each source is created. This thread then updates the outputs.
initialize :: Bar SourceSettings -> IO (Bar SourceHandle)
initialize (Source settings) = do
  outputRef <- newIORef ""
  cacheRef <- newIORef Nothing
  threadId <- forkIO (mkThread settings outputRef cacheRef)
  pure $ Source (SourceHandle {..})
initialize (Txt text) = pure $ Txt text
initialize (Marquee i p) = Marquee i <$> initialize p
initialize (Color color p) = Color color <$> initialize p
initialize (Bars ps) = Bars <$> mapM initialize ps
initialize (Raw text) = pure $ Raw text

-- | Run source process either once or forever, depending on source settings.
mkThread :: SourceSettings -> IORef Text -> Cache -> IO ()
mkThread (SourceSettings { command = [] }) outputRef cacheRef = do
  let message = "dzen-dhall error: no command specified"
  writeIORef cacheRef $ Just message
  writeIORef outputRef message
mkThread (SourceSettings { updateInterval, command = (binary : args), stdin }) outputRef cacheRef =
  case updateInterval of

    -- If update interval is specified, loop forever.
    Just interval -> do
      let delay = interval * 1000000

      forever $ do
        runSourceProcess (proc binary args) outputRef cacheRef stdin
        threadDelay delay

    -- If update interval is not specified, run the source once.
    Nothing -> do
      runSourceProcess (proc binary args) outputRef cacheRef stdin

-- | Creates a process, subscribes to its stdout handle and updates the output ref.
runSourceProcess :: CreateProcess -> IORef Text -> Cache -> Maybe Text -> IO ()
runSourceProcess cp outputRef cacheRef mbInput = do
  (mb_stdin_hdl, mb_stdout_hdl, mb_stderr_hdl, _) <- createProcess cp

  case (mb_stdin_hdl, mb_stdout_hdl, mb_stderr_hdl) of
    (Just stdin_hdl, Just stdout_hdl, _) -> do
      hSetBuffering stdin_hdl  LineBuffering
      hSetBuffering stdout_hdl LineBuffering

      -- If the input is specified, write it to the stdin handle
      whenJust mbInput $ \text -> do
        Data.Text.IO.hPutStrLn stdin_hdl text

      -- Loop until EOF, updating outputRef on each line
      loopWhileM (not <$> hIsEOF stdout_hdl) $ do
        line <- Data.Text.IO.hGetLine stdout_hdl
        -- Drop cache
        writeIORef cacheRef Nothing
        writeIORef outputRef line

    _ -> do
      writeIORef outputRef "dzen-dhall error: Couldn't open IO handle(s)"

-- | Produces an AST from 'Bar'.
collectSources :: Bar SourceHandle -> IO AST
collectSources (Source SourceHandle { outputRef, cacheRef })
  = do
  cache <- readIORef cacheRef
  case cache of
    Just escaped ->
      pure $ ASTText escaped
    Nothing -> do
      newText <- readIORef outputRef
      let escaped = escape newText
      writeIORef cacheRef (Just escaped)
      pure $ ASTText escaped
collectSources (Txt text)
  = pure $ ASTText text
collectSources (Marquee speed p)
  = collectSources p -- TODO
collectSources (Color color p)
  = Prop (FG color) <$> collectSources p -- TODO
collectSources (Bars ps)
  = mconcat <$> mapM collectSources ps

-- | Escape formatting and join all lines into one.
escape :: Text -> Text
escape = escapeFormatting . removeNewLines

-- | Remove all formatting from string, so that dzen2 will interpret it literally.
--
-- e.g.
-- @
-- escape "^bg(red) ^^ ^bg()" = "^^bg(red) ^^^^ ^^bg()"
-- @
escapeFormatting :: Text -> Text
escapeFormatting = Data.Text.replace "^" "^^"

-- | Join multiple lines into one (using space character as a separator).
removeNewLines :: Text -> Text
removeNewLines = Data.Text.replace "\n" " "

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
      P position -> spec <> inner
        where
          spec =
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
  "^p(_LOCK_X)" <> spec <> "^p(_UNLOCK_X)^ib(1)" <> padding <> "^ib(0)"
  where
    padding = Data.Text.justifyRight width ' ' ""
    spec =
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
