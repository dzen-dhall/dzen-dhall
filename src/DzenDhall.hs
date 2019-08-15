module DzenDhall where

import           DzenDhall.App
import           DzenDhall.App.Run (useConfigurations)
import           DzenDhall.Arguments
import           DzenDhall.Plug
import           DzenDhall.Runtime

import           Lens.Micro
import           Options.Applicative (execParser)
import           System.Exit (exitWith, ExitCode(..))
import qualified GHC.IO.Encoding
import qualified System.IO


main :: IO ()
main = do

  GHC.IO.Encoding.setLocaleEncoding System.IO.utf8

  arguments <- execParser argumentsParser
  case arguments ^. mbCommand of
    Nothing -> do
      runtime <- readRuntime arguments
      runApp runtime () $ do
        useConfigurations
        waitForever

    Just Init -> do
      initCommand arguments
      exitWith ExitSuccess

    Just (Plug str) -> do
      runtime <- readRuntime arguments
      runApp runtime () $ do
        plugCommand str
        liftIO $ exitWith ExitSuccess
