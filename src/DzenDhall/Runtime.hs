{-# LANGUAGE RecordWildCards #-}
module DzenDhall.Runtime where

import DzenDhall.Arguments
import Paths_dzen_dhall
import           System.Posix.Files
import           System.Directory
import Control.Monad
import System.FilePath ((</>))
import Control.Applicative


data Runtime = Runtime
  { configPath :: String
  }
  deriving (Eq, Show)

mkRuntime :: Arguments -> IO Runtime
mkRuntime Arguments{configPath = configPath'} = do
  configPath <- maybe (getXdgDirectory XdgConfig "dzen-dhall") pure configPath'
  exists <- doesDirectoryExist configPath
  when (not exists) $ do
    putStrLn $ "Configuration directory does not exist, creating " <> configPath
    dataDir <- getDataDir
    createDirectoryIfMissing True configPath

    let mode400 = ownerReadMode
        mode600 = mode400 `unionFileModes` ownerWriteMode
        mode700 = mode600 `unionFileModes` ownerExecuteMode

    copyDir (dataDir </> "dhall") configPath
      (flip setFileMode mode400)
      (flip setFileMode mode700)

    setFileMode configPath mode700
    setFileMode (configPath </> "config.dhall") mode600
  pure $ Runtime {..}

type Hook = FilePath -> IO ()

copyDir :: FilePath -> FilePath -> Hook -> Hook -> IO ()
copyDir src dst fileCreationHook dirCreationHook = do
  content <- listDirectory src
  forM_ content $ \name -> do
    let srcPath = src </> name
    let dstPath = dst </> name
    isDirectory <- doesDirectoryExist srcPath
    if isDirectory
      then do
        createDirectory dstPath
        dirCreationHook dstPath
        copyDir srcPath dstPath fileCreationHook dirCreationHook
      else do
        copyFile srcPath dstPath
        fileCreationHook dstPath

  where
    doesFileOrDirectoryExist x = orM [doesDirectoryExist x, doesFileExist x]
    orM xs = or <$> sequence xs
    whenM s r = s >>= flip when r
