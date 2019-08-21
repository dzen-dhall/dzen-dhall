module DzenDhall.Runtime where

import           DzenDhall.Arguments
import           DzenDhall.Runtime.Data
import           DzenDhall.Config hiding (Hook)
import           Paths_dzen_dhall

import           Control.Monad
import           Data.Maybe
import           Dhall hiding (maybe)
import           Lens.Micro
import           System.Directory
import           System.Exit (ExitCode(..), exitWith)
import           System.FilePath ((</>))
import           System.Posix.Files
import qualified System.Console.ANSI
import qualified System.IO


-- Read runtime from configuration file, if possible.
readRuntime :: Arguments -> IO Runtime
readRuntime args = do
  let dzenBinary = fromMaybe "dzen2" (args ^. mbDzenBinary)

  configDir <- maybe (getXdgDirectory XdgConfig "dzen-dhall") pure (args ^. mbConfigDir)
  exists <- doesDirectoryExist configDir

  unless exists $ do
    putStrLn "Configuration directory does not exist, you should create it first by running `dzen-dhall init`."
    exitWith $ ExitFailure 2

  let configFile = configDir </> "config.dhall"

  putStrLn $ "Reading configuration from " <> configFile

  configurations :: [Configuration] <- do
    (if args ^. explain == Explain
     then detailed
     else id) $ inputFile (list configurationType) configFile

  supportsANSI <- System.Console.ANSI.hSupportsANSI System.IO.stdout

  pure $ Runtime
    configDir
    configurations
    dzenBinary
    args
    supportsANSI


-- | Create config directory and set file permissions.
initCommand :: Arguments -> IO ()
initCommand args = do
  configDir <- maybe (getXdgDirectory XdgConfig "dzen-dhall") pure (args ^. mbConfigDir)

  let pluginsDir = configDir </> "plugins"

  exists <- doesDirectoryExist configDir

  when exists $ do
    putStrLn "Configuration directory already exists."
    exitWith (ExitFailure 1)

  dataDir <- getDataDir

  createDirectoryIfMissing True configDir
  createDirectoryIfMissing True pluginsDir

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

  setFileMode configFile mode600

  putStrLn $ "Success! You can now view your configuration at " <> configFile
  putStrLn $ "Run dzen-dhall again to see it in action."

type FileHook = FilePath -> IO ()

copyDir :: FileHook -> FileHook -> FilePath -> FilePath -> IO ()
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
          createDirectoryIfMissing True dstPath
          dirCreationHook dstPath
          go srcPath dstPath
          else do
          copyFile srcPath dstPath
          fileCreationHook dstPath
