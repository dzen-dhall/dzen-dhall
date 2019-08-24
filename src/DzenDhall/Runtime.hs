module DzenDhall.Runtime where

import           DzenDhall.Arguments
import           DzenDhall.Runtime.Data
import           DzenDhall.Templates (staticFiles)
import           DzenDhall.Config hiding (Hook)

import           Control.Monad
import           Control.Arrow
import           Data.Maybe
import           Dhall hiding (maybe)
import           Lens.Micro
import           System.Directory
import           System.Exit (ExitCode(..), exitWith)
import           System.FilePath ((</>), takeDirectory)
import           System.Posix.Files
import qualified System.Console.ANSI
import qualified System.IO
import qualified Data.ByteString as BS


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
    putStrLn $ "Configuration directory already exists: " <> configDir
    exitWith (ExitFailure 1)

  let mode400 = ownerReadMode
      mode600 = mode400 `unionFileModes` ownerWriteMode

  createDirectoryIfMissing True configDir
  createDirectoryIfMissing True pluginsDir

  forM_ (staticFiles <&> first (configDir <>)) \(file, contents) -> do
    createDirectoryIfMissing True (takeDirectory file)
    BS.writeFile file contents
    setFileMode file mode400

  let configFile = configDir </> "config.dhall"

  setFileMode configFile mode600

  putStrLn $ "Success! You can now view your configuration at " <> configFile
  putStrLn $ "Run dzen-dhall again to see it in action."
