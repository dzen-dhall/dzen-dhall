module DzenDhall.Commands.Validate where

import           DzenDhall.App
import           DzenDhall.App as App
import           DzenDhall.Arguments
import           DzenDhall.Config
import           DzenDhall.Extra
import           DzenDhall.Runtime.Data
import qualified DzenDhall.Parser as Parser
import qualified DzenDhall.Validation as Validation

import           Control.Monad
import           Lens.Micro
import           Lens.Micro.Extras

validateCommand :: ValidateCommand -> App Common ()
validateCommand (ValidateCommand skipAssertions) = do
  runtime <- App.getRuntime

  forM_ (view rtConfigurations runtime) \cfg -> do

    let barTokens = cfg ^. cfgBarTokens

    when (skipAssertions == DontSkip) do
      assertionErrors <- liftIO do
        Validation.checkAssertions barTokens

      unless (null assertionErrors) do
        App.exit 3 $ Validation.report assertionErrors

    let errors = Validation.validate barTokens

    unless (null errors) do
      App.exit 3 $ Validation.report errors

    let parseResult =
          Parser.runBarParser $
          Validation.filterOutAssertions barTokens

    whenLeft parseResult
      (exitWithError barTokens)

exitWithError :: Show a => [Token] -> a -> App Common b
exitWithError barTokens err =
  App.exit 3 $ fromLines
  [ "Internal error when parsing configuration, debug info: " <> showPack barTokens
  , "Error: " <> showPack err
  , "Please report as bug."
  ]
