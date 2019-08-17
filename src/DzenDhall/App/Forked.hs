module DzenDhall.App.Forked where

import           DzenDhall.AST.Render
import           DzenDhall.App as App
import           DzenDhall.App.StartingUp
import           DzenDhall.Config
import           DzenDhall.Data
import           DzenDhall.Runtime.Data

import           Control.Concurrent
import           Control.Monad
import           Data.Maybe
import           Lens.Micro
import qualified Data.Text.IO

updateForever
  :: Bar Initialized
  -> App Forked ()
updateForever bar = do

  barRuntime <- get

  let barSettings = barRuntime ^. brConfiguration ^. cfgBarSettings
      fontWidth = fromMaybe 10 $ barSettings ^. bsFontWidth

  forever $ do
    output <- runRender <$>
      collectSources fontWidth bar
    liftIO $ do
      Data.Text.IO.hPutStrLn (barRuntime ^. brHandle) output
      threadDelay $ barSettings ^. bsUpdateInterval
    modify $ brFrameCounter +~ 1
