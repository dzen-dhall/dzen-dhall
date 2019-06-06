module DzenDhall where

import Options.Applicative (execParser)
import System.Exit (exitWith, ExitCode(..))

import DzenDhall.Arguments (Arguments(..), Command(..), argumentsParser)
import DzenDhall.Runtime (initCommand, readRuntime)
import DzenDhall.Run (useConfigurations)
import qualified Control.Concurrent.Async as Async

main :: IO ()
main = do
  arguments <- execParser argumentsParser
  case mbCommand arguments of
    Nothing -> do
      runtime <- readRuntime arguments
      asyncs <- useConfigurations runtime
      mapM_ Async.wait asyncs

    Just Init -> do
      initCommand arguments
      exitWith ExitSuccess
