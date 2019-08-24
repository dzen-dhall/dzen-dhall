module DzenDhall.Commands.Plug where

import           DzenDhall.App
import           DzenDhall.Config
import           DzenDhall.Extra
import           DzenDhall.Runtime.Data
import           DzenDhall.Arguments (PlugCommand (..), ConfirmPlug(..))

import           Control.Applicative
import           Control.Exception hiding (try)
import           Control.Monad
import           Control.Monad.Trans.Except
import           Data.Either
import           Data.Maybe
import           Data.Text (Text)
import           Dhall
import           Dhall.Core
import           Lens.Micro
import           Network.HTTP.Client.Conduit
import           Network.HTTP.Simple
import           Network.HTTP.Types
import           Network.URI
import           Prelude
import           System.Directory
import           System.Exit
import           System.FilePath ((</>))
import qualified Data.ByteString.UTF8                      as BSU
import qualified Data.Text                                 as T
import qualified Data.Text.Encoding                        as STE
import qualified Data.Text.IO
import qualified Data.Text.Prettyprint.Doc                 as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as Pretty.Terminal
import qualified Dhall.Parser                              as Dhall
import qualified Dhall.Pretty
import qualified Dhall.Pretty                              as Pretty
import qualified System.IO
import qualified Text.Megaparsec                           as MP
import qualified Text.Parsec                               as P


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


plugCommand :: PlugCommand -> App Common ()
plugCommand (PlugCommand spec shouldConfirm) = do

  withEither (parseSourceSpec spec) invalidSourceSpec $ \sourceSpec -> do
    rawContents <- liftIO $
      getPluginContents sourceSpec

    withMaybe (tryDhallParse rawContents) (invalidFile rawContents) $ \expr -> do

      eiMeta <- readPluginMeta sourceSpec rawContents

      withEither eiMeta handleMetaValidationError $ \meta -> do

        let pluginName = meta ^. pmName

        checkIfPluginFileExists pluginName

        when (shouldConfirm == Confirm) $ do
          suggestReviewing expr

          askConfirmation

        writePluginFile pluginName rawContents

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
httpHandler exception = do
  Data.Text.IO.putStrLn "Exception occured while trying to load plugin source:"
  case exception of
    HttpExceptionRequest _request (StatusCodeException response _) -> do
      let status  = getResponseStatus response
          code    = getResponseStatusCode response
          message = BSU.toString $ statusMessage status
      putStrLn $ "Error: " <> show code <> ", " <> message

    _ -> print exception
  exitWith $ ExitFailure 1


parseSourceSpec :: String -> Either P.ParseError PluginSourceSpec
parseSourceSpec spec = P.runParser sourceSpecParser () spec spec
  where

    sourceSpecParser :: P.Parsec String () PluginSourceSpec
    sourceSpecParser = ( P.try fromURL
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


saneIdentifier :: P.Parsec String () String
saneIdentifier = P.many1 (P.alphaNum <|> P.char '-' <|> P.char '_')

getPluginSource :: PluginSourceSpec -> String
getPluginSource FromGithub{userName, repository, revision} =
  "https://raw.githubusercontent.com/" <> userName
  <> "/" <> repository
  <> "/" <> revision
  <> "/plugin.dhall"
getPluginSource (FromOrg { name, revision }) =
  "https://raw.githubusercontent.com/dzen-dhall/plugins/" <> revision
  <> "/" <> name
  <> "/plugin.dhall"
getPluginSource (FromURL url) = do
  uriToString id url ""
getPluginSource (FromFile filePath) =
  filePath


getPluginContents :: PluginSourceSpec -> IO Text
getPluginContents (FromFile filePath)  =
  Data.Text.IO.readFile filePath
getPluginContents other =
  getPluginContentsFromURL $ getPluginSource other


-- | Load plugin from URL and validate it by parsing.
getPluginContentsFromURL :: String -> IO Text
getPluginContentsFromURL url = handle httpHandler $ do

  req <- parseUrlThrow url
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
readPluginMeta :: PluginSourceSpec -> Text -> App Common (Either MetaValidationError PluginMeta)
readPluginMeta sourceSpec contents = do
  dhallDir <- getRuntime <&> (^. rtConfigDir)

  -- A dirty hack - we just access the `meta` field, discarding `main`
  -- so that there is no need to deal with its type.
  let metaBody      = "(" <> contents <> ").meta"
      inputSettings =
        defaultInputSettings &
        rootDirectory .~ (dhallDir </> "plugins") &
        sourceName    .~ getPluginSource sourceSpec


  -- TODO: convert exception to `NoParse`
  meta <- explained $ inputWithSettings inputSettings pluginMetaType metaBody

  let name          = meta ^. pmName
      parseResult   = P.runParser saneIdentifier () "Plugin name" (T.unpack name)

  liftIO $ runExceptT $ do

    -- TODO: add more validations

    unless (isRight parseResult) $ do
      throwE (InvalidPluginName name)

    pure meta


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
