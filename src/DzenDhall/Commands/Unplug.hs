module DzenDhall.Commands.Unplug where

import           DzenDhall.App
import           DzenDhall.Extra
import           DzenDhall.Arguments
import qualified DzenDhall.Commands.Plug as Plug

import           Control.Applicative
import           Control.Monad
import           Data.Text (Text)
import           Prelude
import           System.Directory (removeFile)
import qualified Data.Text as T
import qualified Text.Parsec as P


unplugCommand :: UnplugCommand -> App Common ()
unplugCommand (UnplugCommand argument) = do

  let parseResult =
        T.pack <$> P.runParser Plug.saneIdentifier () "Plugin name" argument

  withEither parseResult
    invalidPluginName
    \pluginName -> do

      Plug.checkIfPluginFileExists pluginName

      echo =<< highlight "Are you sure you want to remove this plugin? (Y/n)"

      response <- liftIO getLine

      unless (isYes response) do
        exit 1 "Aborting."

      deletePluginFile pluginName

      echo =<< highlight
        "Success! You should delete all references of this plugin from your config file."


invalidPluginName :: P.ParseError -> App Common ()
invalidPluginName err =
  exit 1 $ fromLines
  [ "Invalid plugin name"
  , ""
  , showPack err
  ]


deletePluginFile :: Text -> App Common ()
deletePluginFile pluginName = do
  pluginFile <- snd <$> Plug.getPluginPaths pluginName
  liftIO $ removeFile pluginFile
