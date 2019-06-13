module DzenDhall.Plug where

import           Control.Applicative
import           Control.Exception hiding (try)
import           Control.Monad
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Except
import           Data.HashSet (member)
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as STE
import           Data.Text.IO (putStrLn, writeFile)
import           Dhall
import           Dhall.Core
import qualified Dhall.Parser as Dhall
import           DzenDhall.App
import           DzenDhall.Config
import           DzenDhall.Runtime
import           Lens.Micro
import           Network.HTTP.Simple
import           Network.URI
import           Prelude hiding (putStrLn, takeWhile, writeFile)
import           System.Directory
import           System.Exit
import           System.FilePath ((</>))
import qualified Text.Megaparsec
import qualified Text.Megaparsec as MP
import qualified Text.Parsec as P
import qualified Text.Parser.Combinators (try)

data Plugin
  = FromGithub
  -- ^ Load from Github by username and repository
    { userName   :: String
    , repository :: String
    }

  | FromOrg String
  -- ^ Load from @https://github.com/dzen-dhall/plugins@ by name.

  | FromURL URI
  -- ^ Load from URL
  deriving (Eq, Show)


plugCommand :: String -> App ()
plugCommand argument = do

  runtime <- getRuntime

  let version  = runtime ^. rtAPIVersion
      dhallDir = runtime ^. rtConfigDir

  liftIO $ handle httpHandler $ do

    case runPluginParser argument of

      Left err -> do
        putStrLn "Error while parsing URL:"
        print err
        exitWith $ ExitFailure 1

      Right plugin -> do
        let url = getPluginURL plugin version

        putStrLn $ "Trying to load plugin from URL: " <> T.pack url <> "\n"

        bodyText <- loadPluginContents url

        when (isNothing $ tryDhallParse bodyText) $ do
          putStrLn "Error: not a valid Dhall file!"
          putStrLn bodyText
          exitWith $ ExitFailure 2

        -- TODO: ask for confirmation
        putStrLn "Please review the code:\n\n"
        putStrLn bodyText

        eiMeta <- readPluginMeta dhallDir bodyText

        case eiMeta of

          Left err -> do
            putStrLn $ printMetaValidationError err
            exitWith $ ExitFailure 2

          Right meta -> do
            writePluginFile dhallDir (meta ^. pmName) bodyText

            let usage = "New plugin \"" <> meta ^. pmName
                        <> "\" can now be used as follows:\n\n"
                        <> meta ^. pmUsage

            putStrLn usage


httpHandler :: HttpException -> IO a
httpHandler err = do
  putStrLn "Exception occured while trying to load plugin source:"
  print err
  exitWith $ ExitFailure 1


runPluginParser :: String -> Either P.ParseError Plugin
runPluginParser = P.runParser pluginParser () "input"


pluginParser :: P.Parsec String () Plugin
pluginParser = ( P.try fromURL
             <|> P.try fromGithub
             <|> P.try fromOrg
               ) <* P.eof
  where

    fromGithub :: P.Parsec String () Plugin
    fromGithub = do
      userName <- P.many1 P.alphaNum
      void $ P.char '/'
      repository <- P.many1 P.alphaNum
      pure $ FromGithub {..}

    fromOrg :: P.Parsec String () Plugin
    fromOrg = FromOrg <$> P.many1 P.alphaNum

    fromURL :: P.Parsec String () Plugin
    fromURL = do
      anything <- P.many1 (P.satisfy (const True))
      case parseURI anything of
        Just uri -> pure $ FromURL uri
        Nothing -> P.unexpected "Not a URI"


-- | Construct plugin source URL by given Plugin and API version.
getPluginURL :: Plugin -> Int -> String
getPluginURL FromGithub{userName, repository} version =
  "https://raw.githubusercontent.com/" <> userName <> "/" <> repository <> "/master/v" <> show version <> "/plugin.dhall"
getPluginURL (FromOrg name) version =
  "https://raw.githubusercontent.com/dzen-dhall/plugins/master/" <> name <> "/v" <> show version <> "/plugin.dhall"
getPluginURL (FromURL url) _version =
  uriToString id url ""


-- | Load plugin from URL and validate it by parsing.
loadPluginContents :: String -> IO Text
loadPluginContents url = do

  req <- parseRequest url `catch` httpHandler
  res <- httpBS req

  let body     = getResponseBody res
      bodyText = STE.decodeUtf8 body

  pure bodyText


-- | Try parsing a file.
tryDhallParse :: Text -> Maybe (Expr Dhall.Src Import)
tryDhallParse =
  MP.parseMaybe (Dhall.unParser Dhall.expr)


data MetaValidationError
  = InvalidAPIVersion { _mveExpected :: Int
                      , _mveGot      :: Int
                      }
  | NoParse
  | InvalidPluginName Text

printMetaValidationError :: MetaValidationError -> Text
printMetaValidationError (InvalidAPIVersion expected got)
  =  "This plugin is written with the use of incompatible API version: expected v"
  <> T.pack (show expected)
  <> ", got v"
  <> T.pack (show got)
printMetaValidationError NoParse
  =  "No parse"
printMetaValidationError (InvalidPluginName name)
  =  "Plugin name should be a valid dhall identifier: " <> name

-- | Try to load plugin meta by inserting a plugin into the current environment
-- (using 'inputWithSettings').
readPluginMeta :: String -> Text -> IO (Either MetaValidationError PluginMeta)
readPluginMeta dhallDir bodyText = runExceptT $ do

  -- A dirty hack - we just access the `meta` field, discarding `main`
  -- so that there is no need to deal with its type.
  let metaBody      = "(" <> bodyText <> ").meta"
      inputSettings =
        defaultInputSettings &
        rootDirectory .~ (dhallDir </> "plugins")

  -- TODO: convert exception to `NoParse`
  meta <- lift $ detailed $ inputWithSettings inputSettings pluginMetaType metaBody

  let name          = meta ^. pmName
      gotApiVersion = meta ^. pmApiVersion
      parseResult   = MP.parseMaybe (Dhall.unParser simpleLabel) name

  unless (isJust parseResult) $ do
    throwE (InvalidPluginName name)

  unless (meta ^. pmApiVersion == apiVersion) $ do
    throwE (InvalidAPIVersion apiVersion gotApiVersion)

  pure meta

-- | Parser for valid variable names. Borrowed from `Dhall.Parser.Token`,
-- slightly modified.
simpleLabel :: Dhall.Parser Text
simpleLabel = Text.Parser.Combinators.try $ do
    c    <- Dhall.Parser $ Text.Megaparsec.satisfy headCharacter
    rest <- takeWhile tailCharacter
    let text = T.cons c rest
    Control.Monad.guard (not $ member text reservedIdentifiers)
    return text
  where
    headCharacter c = alpha c || c == '_'
    tailCharacter c = alpha c || digit c || c == '_' || c == '-' || c == '/'

    alpha :: Char -> Bool
    alpha c = ('\x41' <= c && c <= '\x5A') || ('\x61' <= c && c <= '\x7A')

    digit :: Char -> Bool
    digit c = '\x30' <= c && c <= '\x39'

    takeWhile :: (Char -> Bool) -> Dhall.Parser Text
    takeWhile predicate = Dhall.Parser (Text.Megaparsec.takeWhileP Nothing predicate)


writePluginFile :: FilePath -> Text -> Text -> IO ()
writePluginFile dhallDir name bodyText = do

  let pluginsDir = dhallDir </> "plugins"
      pluginFile = pluginsDir </> T.unpack name <> ".dhall"

  dirExists <- doesDirectoryExist pluginsDir
  unless dirExists $ do
    putStrLn $
      "Directory "
      <> T.pack pluginsDir
      <> " does not exist! Run `dzen-dhall init` first."
    exitWith $ ExitFailure 1

  fileExists <- doesFileExist pluginFile
  when fileExists $ do
    putStrLn $ "File " <> T.pack pluginFile <> " already exists."
    exitWith $ ExitFailure 1

  writeFile pluginFile bodyText
