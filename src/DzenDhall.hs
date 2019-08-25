module DzenDhall where

import           DzenDhall.App
import           DzenDhall.App.Run (useConfigurations)
import           DzenDhall.Arguments
import           DzenDhall.Commands.Plug
import           DzenDhall.Commands.Unplug
import           DzenDhall.Commands.Validate
import           DzenDhall.Runtime
import           DzenDhall.Extra (waitForever)

import           Lens.Micro
import           Options.Applicative (execParser)
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
        liftIO waitForever

    Just Init -> do
      initCommand arguments

    Just (Plug commandArgs) -> do
      runtime <- readRuntime arguments
      runApp runtime () $
        plugCommand commandArgs

    Just (Unplug commandArgs) -> do
      runtime <- readRuntime arguments
      runApp runtime () $
        unplugCommand commandArgs

    Just (Validate commandArgs) -> do
      runtime <- readRuntime arguments
      runApp runtime () (validateCommand commandArgs)
