module DzenDhall.App.Run where

import           DzenDhall.App as App
import           DzenDhall.App.StartingUp
import           DzenDhall.App.Forked
import           DzenDhall.Config
import           DzenDhall.Data
import           DzenDhall.Event
import           DzenDhall.Extra
import           DzenDhall.Runtime
import           DzenDhall.Validation
import qualified DzenDhall.Parser as Parser

import           Control.Concurrent
import           Control.Monad
import           Lens.Micro
import           Lens.Micro.Extras

-- | Parses 'Bar's. For each 'Configuration' spawns its own dzen binary.
useConfigurations :: App Common ()
useConfigurations = do
  runtime <- App.getRuntime
  forM_ (view rtConfigurations runtime) $ \cfg -> do
    let barTokens = cfg ^. cfgBarTokens
    let errors = validate barTokens

    unless (null errors) $
      App.exit 3 $ report errors

    case Parser.runBarParser barTokens of
      Left err -> App.exit 3 $ fromLines
        [ "Internal error when parsing configuration, debug info: " <> showPack barTokens
        , "Error: " <> showPack err
        , "Please report as bug."
        ]

      Right (bar :: Bar Marshalled) -> do

        let barSettings = cfg ^. cfgBarSettings

        (bar' :: Bar Initialized,
         handles :: AutomataHandles,
         barRuntime :: BarRuntime) <-
          liftStartingUp (startUp cfg bar) barSettings

        void $ liftIO $ forkIO $
          launchEventListener (barRuntime ^. brNamedPipe) handles

        runAppForked (updateForever bar') barRuntime
