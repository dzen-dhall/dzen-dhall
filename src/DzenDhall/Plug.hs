module DzenDhall.Plug where

import           DzenDhall.App
import           DzenDhall.Config
import           DzenDhall.Extra
import           DzenDhall.Runtime.Data

import           Control.Applicative
import           Control.Exception hiding (try)
import           Control.Monad
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Except
import           Data.HashSet (member)
import           Data.Maybe
import           Data.Text (Text)
import           Dhall
import           Dhall.Core
import           Lens.Micro
import           Network.HTTP.Simple
import           Network.URI
import           Prelude
import           System.Directory
import           System.Exit
import           System.FilePath ((</>))
import qualified Data.Text                                 as T
import qualified Data.Text.Encoding                        as STE
import qualified Data.Text.IO
import qualified Data.Text.Prettyprint.Doc                 as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as Pretty.Terminal
import qualified Dhall.Parser                              as Dhall
import qualified Dhall.Pretty
import qualified Dhall.Pretty                              as Pretty
import qualified System.IO
import qualified Text.Megaparsec
import qualified Text.Megaparsec                           as MP
import qualified Text.Parsec                               as P
import qualified Text.Parser.Combinators (try)


data PluginSourceSpec
  = FromGithub
  -- ^ Load from Github by username and repository
    { userName   :: String
    , repository :: String
    , revision   :: String
    }
  | FromOrg { name     :: String
            , revision :: String
            }
  -- ^ Load from @https://github.com/dzen-dhall/plugins@ by name.
  | FromURL URI
  -- ^ Load from URL
  | FromFile FilePath
  deriving (Eq, Show)


plugCommand :: String -> App Common ()
plugCommand argument = do

  withEither (parseSourceSpec argument) invalidSourceSpec $ \sourceSpec -> do
    runtime <- getRuntime

    let version  = runtime ^. rtAPIVersion

    rawContents <- liftIO $
      handle httpHandler $ getPluginContents sourceSpec version

    withMaybe (tryDhallParse rawContents) (invalidFile rawContents) $ \expr -> do

      eiMeta <- readPluginMeta rawContents

      withEither eiMeta handleMetaValidationError $ \meta -> do

        checkIfPluginFileExists (meta ^. pmName)

        suggestReviewing expr

        askConfirmation

        writePluginFile (meta ^. pmName) rawContents

        printUsage meta


askConfirmation :: App Common ()
askConfirmation = do

  msg <- highlight "Are you sure you want to install this plugin? (Y/n)"
  echo msg

  response <- liftIO getLine
  unless (response `elem` ["Y", "y", "Yes", "yes", ""]) $
    exit 1 "Aborting."

suggestReviewing :: Expr Dhall.Src Import -> App Common ()
suggestReviewing expr = do
  msg1 <- highlight "Please review the plugin code:"
  msg2 <- highlight "Please review the code above."

  let expr' =
        Dhall.Pretty.prettyCharacterSet Dhall.Pretty.Unicode expr

  echoLines [ ""
            , msg1
            , ""
            ]

  supportsANSI <- getRuntime <&> (^. rtSupportsANSI)

  liftIO $
    if supportsANSI
    then
      Pretty.Terminal.renderIO
      System.IO.stdout
      (fmap Pretty.annToAnsiStyle (Pretty.layoutSmart Pretty.layoutOpts expr'))
    else
      Pretty.Terminal.renderIO
      System.IO.stdout
      (Pretty.layoutSmart Pretty.layoutOpts (Pretty.unAnnotate expr'))

  echoLines [ ""
            , ""
            , msg2
            , ""
            ]

printUsage :: PluginMeta -> App Common ()
printUsage meta = do
  msg <- highlight $
         "New plugin \"" <> meta ^. pmName <> "\" can now be used as follows:"
  echoLines [ msg
            , ""
            , ""
            , meta ^. pmUsage
            ]

invalidSourceSpec :: P.ParseError -> App Common ()
invalidSourceSpec err =
  exit 1 $ fromLines
  [ "Invalid plugin source specification:"
  , ""
  , showPack err
  ]

invalidFile :: Text -> App Common ()
invalidFile rawContents =
  exit 2 $ fromLines
  [ "Error: not a valid Dhall file:"
  , ""
  , rawContents
  ]


httpHandler :: HttpException -> IO a
httpHandler err = do
  Data.Text.IO.putStrLn "Exception occured while trying to load plugin source:"
  print err
  exitWith $ ExitFailure 1


parseSourceSpec :: String -> Either P.ParseError PluginSourceSpec
parseSourceSpec = P.runParser pluginSourceSpecParser () "input"
  where

    pluginSourceSpecParser :: P.Parsec String () PluginSourceSpec
    pluginSourceSpecParser = ( P.try fromURL
                           <|> P.try fromGithub
                           <|> P.try fromOrg
                           <|> P.try fromFile
                             ) <* P.eof

    fromURL :: P.Parsec String () PluginSourceSpec
    fromURL = do
      anything <- P.many1 (P.satisfy (const True))
      case parseURI anything of
        Just uri -> pure $ FromURL uri
        Nothing -> P.unexpected "Not a URI"

    fromGithub :: P.Parsec String () PluginSourceSpec
    fromGithub = do
      userName <- saneIdentifier
      void $ P.char '/'
      repository <- saneIdentifier
      revision <- revisionParser
      pure $ FromGithub {..}

    fromOrg :: P.Parsec String () PluginSourceSpec
    fromOrg = do
      name <- saneIdentifier
      revision <- revisionParser
      pure $ FromOrg {..}

    fromFile :: P.Parsec String () PluginSourceSpec
    fromFile = do
      FromFile <$>
        liftM2 (:) (P.char '.' <|> P.char '/') (P.many1 $ P.satisfy (const True))

    revisionParser = P.option "master" $ do
      void $ P.char '@'
      P.many1 (P.alphaNum <|> P.char '-' <|> P.char '_' <|> P.char '.')

    saneIdentifier = P.many1 (P.alphaNum <|> P.char '-' <|> P.char '_')

getPluginContents :: PluginSourceSpec -> Int -> IO Text
getPluginContents FromGithub{userName, repository, revision} version =
  getPluginContentsFromURL $
  "https://raw.githubusercontent.com/" <> userName
  <> "/" <> repository
  <> "/" <> revision
  <> "/v" <> show version
  <> "/plugin.dhall"

getPluginContents (FromOrg { name, revision }) version =
  getPluginContentsFromURL $
  "https://raw.githubusercontent.com/dzen-dhall/plugins/" <> revision
  <> "/" <> name
  <> "/v" <> show version
  <> "/plugin.dhall"

getPluginContents (FromURL url) _version = do
  getPluginContentsFromURL $ uriToString id url ""

getPluginContents (FromFile filePath) _version =
  Data.Text.IO.readFile filePath


-- | Load plugin from URL and validate it by parsing.
getPluginContentsFromURL :: String -> IO Text
getPluginContentsFromURL url = do

  req <- parseRequest url `catch` httpHandler
  res <- httpBS req

  let body     = getResponseBody res
      contents = STE.decodeUtf8 body

  pure contents


-- | Try parsing a file.
tryDhallParse :: Text -> Maybe (Expr Dhall.Src Import)
tryDhallParse =
  MP.parseMaybe (Dhall.unParser Dhall.expr)


data MetaValidationError
  = NoParse
  | InvalidPluginName Text


instance Show MetaValidationError where
  show NoParse =
    "No parse. This plugin is either malformed or was written for another API version. (current API version: " <> show apiVersion <> ")"

  show (InvalidPluginName pluginName) =
    "Plugin name must be a valid Dhall identifier: " <> T.unpack pluginName


handleMetaValidationError :: MetaValidationError -> App stage ()
handleMetaValidationError err = do
  exit 2 (showPack err)


-- | Try to load plugin meta by inserting a plugin into the current environment
-- (using 'inputWithSettings').
readPluginMeta :: Text -> App Common (Either MetaValidationError PluginMeta)
readPluginMeta contents = do
  dhallDir <- getRuntime <&> (^. rtConfigDir)

  liftIO $ runExceptT $ do

    -- A dirty hack - we just access the `meta` field, discarding `main`
    -- so that there is no need to deal with its type.
    let metaBody      = "(" <> contents <> ").meta"
        inputSettings =
          defaultInputSettings &
          rootDirectory .~ (dhallDir </> "plugins")

    -- TODO: convert exception to `NoParse`
    meta <- lift $ detailed $ inputWithSettings inputSettings pluginMetaType metaBody

    let name          = meta ^. pmName
        parseResult   = MP.parseMaybe (Dhall.unParser simpleLabel) name

    unless (isJust parseResult) $ do
      throwE (InvalidPluginName name)

    pure meta

-- | Parser for valid variable names. Borrowed from `Dhall.Parser.Token`,
-- slightly modified.
simpleLabel :: Dhall.Parser Text
simpleLabel = Text.Parser.Combinators.try $ do
    c    <- Dhall.Parser $ Text.Megaparsec.satisfy headCharacter
    rest <- takeWhile' tailCharacter
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

    takeWhile' :: (Char -> Bool) -> Dhall.Parser Text
    takeWhile' predicate = Dhall.Parser (Text.Megaparsec.takeWhileP Nothing predicate)


checkIfPluginFileExists :: Text -> App Common ()
checkIfPluginFileExists pluginName = do
  (pluginsDir, pluginFile) <- getPluginPaths pluginName

  dirExists <- liftIO $ doesDirectoryExist pluginsDir

  unless dirExists $ do
    exit 1 $
      "Directory " <> T.pack pluginsDir <> " does not exist! Run `dzen-dhall init` first."

  fileExists <- liftIO $ doesFileExist pluginFile

  when fileExists $ do
    exit 1 $
      "File " <> T.pack pluginFile <> " already exists."

writePluginFile :: Text -> Text -> App Common ()
writePluginFile pluginName contents = do
  pluginFile <- snd <$> getPluginPaths pluginName
  liftIO $ Data.Text.IO.writeFile pluginFile contents

-- | Given a plugin name, get the file of the plugin and locate the plugins directory.
getPluginPaths :: Text -> App Common (String, String)
getPluginPaths pluginName = do
  dhallDir <- getRuntime <&> (^. rtConfigDir)

  let pluginsDir = dhallDir </> "plugins"
      pluginFile = pluginsDir </> T.unpack pluginName <> ".dhall"

  pure (pluginsDir, pluginFile)
