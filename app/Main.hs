{-# LANGUAGE RecordWildCards #-}
module Main where

import           Options.Applicative (execParser)
import           DzenDhall.Arguments (Arguments(..))
import qualified DzenDhall.Arguments as A
import           DzenDhall.Runtime (Runtime(..))
import qualified DzenDhall.Runtime as R
import           System.Directory
import           Data.Maybe
import           Control.Monad
import           System.Posix.Files
import           Paths_dzen_dhall

mkRuntime :: Arguments -> IO Runtime
mkRuntime Arguments{configPath = configPath'} = do
  configPath <- maybe (getXdgDirectory XdgConfig "dzen-dhall") pure configPath'
  exists <- doesDirectoryExist configPath
  when (not exists) $ do
    putStrLn $ "Configuration directory does not exist, creating " <> configPath
    createDirectoryIfMissing True configPath
    let mode700 = ownerReadMode  `unionFileModes`
                  ownerWriteMode `unionFileModes`
                  ownerExecuteMode
    setFileMode configPath mode700
    headerFile <- getDataFileName "dhall/header.dhall"
    defaultConfigFile <- getDataFileName "dhall/config"

  pure $ Runtime {..}

main :: IO ()
main = do
  arguments <- execParser A.parserInfo
  print arguments
  runtime <- mkRuntime arguments
  print runtime
  putStrLn "Hello, Haskell!"
