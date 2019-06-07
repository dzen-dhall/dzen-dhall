{-# LANGUAGE TemplateHaskell #-}
module DzenDhall.Runtime where

import Control.Monad
import Data.Maybe
import Dhall hiding (maybe)
import DzenDhall.Arguments
import DzenDhall.Config
import Lens.Micro
import Lens.Micro.TH
import Paths_dzen_dhall
import System.Directory
import System.Exit (ExitCode(..), exitWith)
import System.FilePath ((</>))
import System.Posix.Files

data Runtime = Runtime
  { _rtConfigDir :: String
  , _rtConfigurations :: [Configuration]
  , _rtDzenBinary :: String
  , _rtFrameCounter :: Int
  }
  deriving (Eq, Show)

makeLenses ''Runtime

-- Read runtime from configuration file, if possible.
readRuntime :: Arguments -> IO Runtime
readRuntime Arguments{mbConfigDir, mbDzenBinary} = do
  let dzenBinary = fromMaybe "dzen2" mbDzenBinary
  let frameCounter = 0

  configDir <- maybe (getXdgDirectory XdgConfig "dzen-dhall") pure mbConfigDir
  exists <- doesDirectoryExist configDir

  unless exists $ do
    putStrLn "Configuration directory does not exist, you should create it first by running `dzen-dhall init`."
    exitWith $ ExitFailure 2

  let configFile = configDir </> "config.dhall"

  configurations :: [Configuration] <- do
    detailed $ inputFile (list configurationType) configFile

  pure $ Runtime
    configDir
    configurations
    dzenBinary
    frameCounter

-- | Create config directory and set file permissions.
initCommand :: Arguments -> IO ()
initCommand Arguments{mbConfigDir} = do
  configDir <- maybe (getXdgDirectory XdgConfig "dzen-dhall") pure mbConfigDir
  exists <- doesDirectoryExist configDir

  when exists $ do
    putStrLn "Configuration directory already exists."
    exitWith (ExitFailure 1)

  dataDir <- getDataDir
  createDirectoryIfMissing True configDir

  let mode400 = ownerReadMode
      mode600 = mode400 `unionFileModes` ownerWriteMode
      mode700 = mode600 `unionFileModes` ownerExecuteMode

  copyDir
    -- for files
    (`setFileMode` mode400)
    -- for directories
    (`setFileMode` mode700)
    (dataDir </> "dhall")
    configDir

  let configFile = configDir </> "config.dhall"

  setFileMode configDir  mode700
  setFileMode configFile mode600

  putStrLn $ "Success! You can now view your configuration at " <> configFile
  putStrLn $ "Run dzen-dhall again to see it in action."

type Hook = FilePath -> IO ()

copyDir :: Hook -> Hook -> FilePath -> FilePath -> IO ()
copyDir fileCreationHook dirCreationHook = go
  where
    go src dst = do
      content <- listDirectory src
      forM_ content $ \name -> do
        let srcPath = src </> name
        let dstPath = dst </> name

        isDir <- doesDirectoryExist srcPath
        if isDir
          then do
          createDirectory dstPath
          dirCreationHook dstPath
          go srcPath dstPath
          else do
          copyFile srcPath dstPath
          fileCreationHook dstPath
