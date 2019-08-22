module DzenDhall.App.Forked where

import           DzenDhall.AST.Render
import           DzenDhall.App
import           DzenDhall.App.StartingUp
import           DzenDhall.Config
import           DzenDhall.Data
import           DzenDhall.Extra
import           DzenDhall.Runtime.Data

import qualified Control.Monad.Trans.State as State
import           Control.Monad.Trans.Class
import           Control.Concurrent
import           Control.Monad
import           Data.Maybe
import           Lens.Micro
import           Time.System
import           Time.Types
import           Data.Hourglass
import qualified Data.Text.IO


-- | Update a given 'Bar' forever, trying to do it in a timely manner.
updateForever
  :: Bar Initialized
  -> App Forked ()
updateForever bar = do

  barRuntime <- get

  let barSettings = barRuntime ^. brConfiguration ^. cfgBarSettings
      fontWidth = fromMaybe 10 $ barSettings ^. bsFontWidth

  initialTime <- liftIO timeCurrentP

  void $ flip State.runStateT initialTime $ forever $ do

    lastTime <- State.get

    let nextTime :: ElapsedP =
              addElapsedP lastTime $
              ElapsedP 0 $
              NanoSeconds $
              fromIntegral $
              (barSettings ^. bsUpdateInterval * 1000)

    State.put nextTime

    output <- lift $ runRender <$>
      collectSources fontWidth bar

    lift $ do
      modify $ brFrameCounter +~ 1

      liftIO $ do
        Data.Text.IO.hPutStrLn (barRuntime ^. brHandle) output

        now <- timeCurrentP

        threadDelay $
          fromIntegral (case nonNegative $ timeGetNanoSeconds $ nextTime - now of
                           NanoSeconds x -> x `div` 1000
                       )


-- This is a workaround,
-- see https://github.com/vincenthz/hs-hourglass/issues/32
addElapsedP :: ElapsedP -> ElapsedP -> ElapsedP
addElapsedP (ElapsedP e1 (NanoSeconds ns1)) (ElapsedP e2 (NanoSeconds ns2)) =
    let notNormalizedNS = ns1 + ns2
        (retainedNS, ns) = notNormalizedNS `divMod` 1000000000
    in  ElapsedP (e1 + e2 + (Elapsed $ Seconds retainedNS)) (NanoSeconds ns)
