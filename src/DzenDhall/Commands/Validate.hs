module DzenDhall.Commands.Validate where

import           DzenDhall.App
import           DzenDhall.App as App
import           DzenDhall.Arguments
import           DzenDhall.Config
import           DzenDhall.Data
import           DzenDhall.Extra
import           DzenDhall.Runtime.Data
import qualified DzenDhall.Parser as Parser
import qualified DzenDhall.Validation

import           Control.Monad
import           Lens.Micro
import           Lens.Micro.Extras

validateCommand :: ValidateCommand -> App Common ()
validateCommand (ValidateCommand skipAssertions) = do
  runtime <- App.getRuntime
  forM_ (view rtConfigurations runtime) $ \cfg -> do

    let barTokens = cfg ^. cfgBarTokens

    when (skipAssertions == DontSkip) $ do
      impureErrors <- liftIO $ do
        DzenDhall.Validation.checkAssertions barTokens

      unless (null impureErrors) $ do
        App.exit 3 $ DzenDhall.Validation.report impureErrors

    let pureErrors = DzenDhall.Validation.validate barTokens

    unless (null pureErrors) $
      App.exit 3 $ DzenDhall.Validation.report pureErrors

    case Parser.runBarParser $
         DzenDhall.Validation.filterOutAssertions barTokens of
      Left err -> App.exit 3 $ fromLines
        [ "Internal error when parsing configuration, debug info: " <> showPack barTokens
        , "Error: " <> showPack err
        , "Please report as bug."
        ]

      Right (_bar :: Bar Marshalled) -> do
        pure ()
