module DzenDhall where

import qualified Control.Concurrent.Async as Async
import           DzenDhall.App as App
import           DzenDhall.Arguments (Arguments(..), Command(..), argumentsParser)
import           DzenDhall.Plug
import           DzenDhall.Run (useConfigurations)
import           DzenDhall.Runtime (initCommand, readRuntime)
import qualified GHC.IO.Encoding
import           Options.Applicative (execParser)
import           System.Exit (exitWith, ExitCode(..))
import qualified System.IO

main :: IO ()
main = do

  GHC.IO.Encoding.setLocaleEncoding System.IO.utf8

  arguments <- execParser argumentsParser
  case mbCommand arguments of
    Nothing -> do
      runtime <- readRuntime arguments
      runApp runtime $ do
        asyncs <- useConfigurations
        liftIO $ mapM_ Async.wait asyncs

    Just Init -> do
      initCommand arguments
      exitWith ExitSuccess

    Just (Plug str) -> do
      runtime <- readRuntime arguments
      runApp runtime $ do
        plugCommand str
        liftIO $ exitWith ExitSuccess
