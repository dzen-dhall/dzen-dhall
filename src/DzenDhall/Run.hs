{-# LANGUAGE NamedFieldPuns #-}
module DzenDhall.Run where

import Control.Concurrent
import Control.Monad
import Data.IORef
import Data.Text (Text)
import Data.Text.IO
import DzenDhall.Config
import DzenDhall.Data
import System.Process
import GHC.IO.Handle

initialize :: Plugin SourceSettings -> IO (Plugin SourceHandle)
initialize (Source settings) = do
  outputRef <- newIORef ""
  threadId <- forkIO (mkThread settings outputRef)
  pure $ Source (SourceHandle {..})
initialize (Txt text) = pure $ Txt text
initialize (Marquee i p) = Marquee i <$> initialize p
initialize (Color color p) = Color color <$> initialize p
initialize (Plugins ps) = Plugins <$> mapM initialize ps

-- | Run shell command either once or forever.
mkThread :: SourceSettings -> IORef Text -> IO ()
mkThread (SourceSettings { command = [] }) outputRef =
  writeIORef outputRef "dzen-dhall error: no command specified"
mkThread (SourceSettings { updateInterval, command = (binary : args), stdin }) outputRef =
  case updateInterval of

    -- If update interval is specified, loop forever.
    Just interval -> do
      let delay = interval * 1000000

      forever $ do
        runSourceProcess (proc binary args) outputRef stdin
        threadDelay delay

    -- If update interval is not specified, run the source once.
    Nothing -> do
      runSourceProcess (proc binary args) outputRef stdin

runSourceProcess :: CreateProcess -> IORef Text -> Maybe Text -> IO ()
runSourceProcess cp outputRef mbInput = do
  (mb_stdin_hdl, mb_stdout_hdl, mb_stderr_hdl, _) <- createProcess cp

  case (mb_stdin_hdl, mb_stdout_hdl, mb_stderr_hdl) of
    (Just stdin_hdl, Just stdout_hdl, Just stderr_hdl) -> do
      hSetBuffering stdin_hdl  LineBuffering
      hSetBuffering stdout_hdl LineBuffering
      hSetBuffering stderr_hdl LineBuffering

      -- If stdin is specified, write it to the handle
      whenJust mbInput $ \text -> do
        Data.Text.IO.hPutStrLn stdin_hdl text

      -- Loop until EOF, updating outputRef on each line
      loopWhileM (not <$> hIsEOF stdout_hdl) $ do
        line <- Data.Text.IO.hGetLine stdout_hdl
        writeIORef outputRef line

    _ -> do
      writeIORef outputRef "dzen-dhall error: Couldn't open IO handle(s)"

collectSources :: Plugin SourceHandle -> IO AST
collectSources (Source SourceHandle { outputRef })
  = ASTText <$> readIORef outputRef
collectSources (Txt text)
  = pure $ ASTText text
collectSources (Marquee speed p)
  = collectSources p -- TODO
collectSources (Color color p)
  = Prop (FG color) <$> collectSources p -- TODO
collectSources (Plugins ps)
  = mconcat <$> mapM collectSources ps

loopWhileM :: Monad m => m Bool -> m () -> m ()
loopWhileM pr act = do
    b <- pr
    when b $ do
      act
      loopWhileM pr act

whenJust :: (Monad m, Monoid b) => Maybe a -> (a -> m b) -> m b
whenJust = flip $ maybe (return mempty)
