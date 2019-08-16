{-# LANGUAGE TypeFamilies #-}
-- | An 'App' monad with its operations.
module DzenDhall.App where

import DzenDhall.Runtime
import DzenDhall.Config

import           Control.Monad.Trans.Class
import qualified Control.Monad.Trans.Reader as Reader
import           Control.Monad.Trans.Reader (ReaderT)
import qualified Control.Monad.Trans.State as State
import           Control.Monad.Trans.State (StateT)
import           Data.Void
import           Data.Text (Text)
import qualified Data.Text
import qualified Data.Text.IO
import           Lens.Micro
import           Control.Concurrent
import           Control.Monad
import           System.Exit
import           System.Random

-- * App execution stages
--
-- $executionStages
--
-- Three newtypes below are used as tags to show in which stage of execution the
-- app currently is.

-- | At this stage, 'App' performs config validation, runs startup/initalization,
-- and forks threads that later update bar outputs.
--
-- See also: 'DzenDhall.App.Run'.
newtype Common     = Common     Void

-- | At this stage, the app initializes 'Source's and launches event listeners
-- that read their corresponding named pipes and handle automata state transitions.
--
-- See also: 'DzenDhall.App.StartingUp'.
newtype StartingUp = StartingUp Void

-- | At this stage, the app only updates bar outputs.
--
-- See also: 'DzenDhall.App.Forked'.
newtype Forked     = Forked     Void

-- | Maps app execution stages to app states.
type family StateOf a

type instance StateOf Common     = ()
type instance StateOf StartingUp = StartupState
type instance StateOf Forked     = BarRuntime

-- | 'Runtime' is read-only; mutable state type depends on the current stage of execution of the 'App'.
newtype App stage a = App { unApp :: StateT (StateOf stage) (ReaderT Runtime IO) a }
  deriving (Functor, Applicative, Monad)

runApp :: Runtime -> StateOf stage -> App stage a -> IO a
runApp rt st app = Reader.runReaderT (State.evalStateT (unApp app) st) rt

liftIO :: IO a -> App stage a
liftIO = App . lift . lift

get :: App stage (StateOf stage)
get = App State.get

put :: StateOf stage -> App stage ()
put = App . State.put

modify :: (StateOf stage -> StateOf stage) -> App stage ()
modify f = get >>= put . f

getRuntime :: App stage Runtime
getRuntime = App $ lift Reader.ask

getCounter :: App StartingUp Int
getCounter = do
  rt <- get
  put $ rt & ssCounter +~ 1
  pure $ rt ^. ssCounter

liftStartingUp :: App StartingUp a -> BarSettings -> App Common a
liftStartingUp (App app) bs = App . lift $ State.evalStateT app initialStartupState
  where
    initialStartupState = StartupState mempty "scope" bs 0 mempty mempty []

runAppForked :: App Forked () -> BarRuntime -> App Common ()
runAppForked app st = do
  rt <- getRuntime
  liftIO $ void $ forkIO $ runApp rt st app

exit :: Int -> Text -> App stage a
exit exitCode message = liftIO $ do
  unless (Data.Text.null message) $
    Data.Text.IO.putStrLn message
  exitWith $ ExitFailure exitCode

waitForever :: App stage a
waitForever = liftIO $ forever $ threadDelay maxBound

randomSuffix :: App stage String
randomSuffix =
  liftIO $ take 10 . randomRs ('a','z') <$> newStdGen
