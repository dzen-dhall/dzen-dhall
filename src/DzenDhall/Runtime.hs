module DzenDhall.Runtime where

import Control.Applicative
import Control.Monad
import DzenDhall.Arguments
import Paths_dzen_dhall
import System.Directory
import System.FilePath ((</>))
import System.Posix.Files


data Runtime = Runtime
  { configDir :: String
  }
  deriving (Eq, Show)

mkRuntime :: Arguments -> IO Runtime
mkRuntime Arguments{mbConfigDir} = do
  configDir <- maybe (getXdgDirectory XdgConfig "dzen-dhall") pure mbConfigDir
  exists <- doesDirectoryExist configDir
  when (not exists) $ do
    putStrLn $ "Configuration directory does not exist, creating " <> configDir
    dataDir <- getDataDir
    createDirectoryIfMissing True configDir

    let mode400 = ownerReadMode
        mode600 = mode400 `unionFileModes` ownerWriteMode
        mode700 = mode600 `unionFileModes` ownerExecuteMode

    copyDir (dataDir </> "dhall") configDir
      (flip setFileMode mode400)
      (flip setFileMode mode700)

    setFileMode configDir mode700
    setFileMode (configDir </> "config.dhall") mode600
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
