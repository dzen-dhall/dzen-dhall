module DzenDhall.App.Forked where

import           DzenDhall.AST.Render
import           DzenDhall.App
import           DzenDhall.App.StartingUp
import           DzenDhall.Config
import           DzenDhall.Data
import           DzenDhall.Runtime.Data

import           Lens.Micro
import qualified Data.Text.IO


-- | Update a given 'Bar' forever, trying to do it in a timely manner.
updateForever
  :: Bar Initialized
  -> App Forked ()
updateForever bar = do

  barRuntime <- get

  let barSettings = barRuntime ^. brConfiguration ^. cfgBarSettings

  forkApp do
    timely (barSettings ^. bsUpdateInterval) do
      output <- runRender <$> collectSources bar
      liftIO $ Data.Text.IO.hPutStrLn (barRuntime ^. brHandle) output
      modify $ brFrameCounter +~ 1
