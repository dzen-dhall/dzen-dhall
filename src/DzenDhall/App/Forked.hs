module DzenDhall.App.Forked where

import           DzenDhall.AST.Render
import           DzenDhall.App
import           DzenDhall.App.StartingUp
import           DzenDhall.Config
import           DzenDhall.Data
import           DzenDhall.Extra
import           DzenDhall.Runtime.Data

import           Control.Concurrent.MVar
import           Control.Exception (try)
import           Lens.Micro
import qualified Data.Text.IO
import qualified GHC.IO.Exception as G


-- | Update a given 'Bar' forever, trying to do it in a timely manner.
updateForever
  :: Bar Initialized
  -> App Forked ()
updateForever bar = do

  barRuntime <- get
  runtime <- getRuntime

  let barSettings = barRuntime ^. brConfiguration ^. cfgBarSettings

  forkApp do
    timely (barSettings ^. bsUpdateInterval) do
      output <- runRender <$> collectSources bar


      liftIO do
        res <- try $ Data.Text.IO.hPutStrLn (barRuntime ^. brHandle) output
        whenLeft res
          \e@(G.IOError {}) -> do
            putStrLn $ "IO error, exiting dzen-dhall: " <> show e
            putMVar (runtime ^. rtExitMVar) ()

      modify $ brFrameCounter +~ 1
