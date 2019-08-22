module DzenDhall where

import           DzenDhall.App
import           DzenDhall.App.Run (useConfigurations)
import           DzenDhall.Arguments
import           DzenDhall.Plug
import           DzenDhall.Validate
import           DzenDhall.Runtime

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
        waitForever

    Just Init -> do
      initCommand arguments

    Just (Plug commandArgs) -> do
      runtime <- readRuntime arguments
      runApp runtime () $
        plugCommand commandArgs

    Just (Validate commandArgs) -> do
      runtime <- readRuntime arguments
      runApp runtime () (validateCommand commandArgs)
