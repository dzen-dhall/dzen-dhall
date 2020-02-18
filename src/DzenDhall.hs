module DzenDhall where

import           DzenDhall.App
import           DzenDhall.App.Run (useConfigurations)
import           DzenDhall.Arguments
import           DzenDhall.Commands.Plug
import           DzenDhall.Commands.Unplug
import           DzenDhall.Commands.Validate
import           DzenDhall.Runtime
import           DzenDhall.Extra (waitForever)

import qualified Paths_dzen_dhall as Paths

import           Data.Version (showVersion)
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
      runApp runtime () do
        useConfigurations
        liftIO waitForever

    Just Init -> do
      initCommand arguments

    Just (Plug commandArgs) -> do
      runtime <- readRuntime arguments
      runApp runtime () do
        plugCommand commandArgs

    Just (Unplug commandArgs) -> do
      runtime <- readRuntime arguments
      runApp runtime () do
        unplugCommand commandArgs

    Just (Validate commandArgs) -> do
      runtime <- readRuntime arguments
      runApp runtime () (validateCommand commandArgs)

    Just Version -> do
      putStrLn $ "v" ++ showVersion Paths.version
