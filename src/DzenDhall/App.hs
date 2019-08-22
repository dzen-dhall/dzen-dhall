{-# LANGUAGE TypeFamilies #-}
-- | An 'App' monad with its operations.
module DzenDhall.App where

import DzenDhall.Runtime.Data
import DzenDhall.Config
import DzenDhall.Arguments
import DzenDhall.Extra

import           Control.Monad.Trans.Class
import qualified Control.Monad.Trans.Reader as Reader
import           Control.Monad.Trans.Reader (ReaderT)
import qualified Control.Monad.Trans.State as State
import           Control.Monad.Trans.State (StateT)
import qualified Data.HashMap.Strict as H
import           Data.Hourglass
import           Data.Void
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text
import qualified Data.Text.IO
import qualified Dhall
import           Lens.Micro
import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           System.Exit
import           System.Random
import           System.Directory
import           System.FilePath ((</>))
import           Time.System


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
  modify $ ssCounter +~ 1
  get <&> (^. ssCounter)

liftStartingUp :: App StartingUp a -> BarSettings -> App Common a
liftStartingUp (App app) barSettings = do

  tmpFilePrefix <- fmap (</> "dzen-dhall-rt-") $ liftIO $
    getTemporaryDirectory `catch` \(_e :: IOException) -> getCurrentDirectory

  -- Let's pretend collisions are impossible.
  -- TODO: handle them.
  namedPipe          <- (tmpFilePrefix <>) <$> randomSuffix
  emitterFile        <- (tmpFilePrefix <>) <$> randomSuffix
  getterFile         <- (tmpFilePrefix <>) <$> randomSuffix
  setterFile         <- (tmpFilePrefix <>) <$> randomSuffix
  variableFilePrefix <- (tmpFilePrefix <>) <$> randomSuffix
  imagePathPrefix    <- (tmpFilePrefix <>) <$> randomSuffix

  let initialStartupState =
        StartupState
        mempty "scope"
        barSettings
        0
        H.empty
        H.empty
        []
        []
        H.empty
        mempty
        namedPipe
        emitterFile
        getterFile
        setterFile
        variableFilePrefix
        imagePathPrefix

  App . lift $ State.evalStateT app initialStartupState

runAppForked :: BarRuntime -> App Forked () -> App Common ()
runAppForked barRuntime app = do
  rt <- getRuntime
  liftIO $ void $ forkIO $ runApp rt barRuntime app

forkApp :: App stage () -> App stage ()
forkApp app = do
  rt <- getRuntime
  st <- get
  void $ liftIO $
    forkIO $ runApp rt st app

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

checkBinary :: String -> App stage Bool
checkBinary = fmap isJust . liftIO . findExecutable

echoLines :: [Text] -> App stage ()
echoLines = mapM_ echo

echo :: Text -> App stage ()
echo = liftIO . Data.Text.IO.putStrLn

highlight :: Text -> App stage Text
highlight text = do
  supportsANSI <- getRuntime <&> (^. rtSupportsANSI)
  pure $
    if supportsANSI then
      "\027[1;33m" <> text <> "\027[0m"
    else
      text

explained :: IO a -> App stage a
explained io = do
  shouldExplain <- getRuntime <&> (\args -> args ^. rtArguments . explain)
  liftIO $ case shouldExplain of
             Explain -> Dhall.detailed io
             DontExplain -> io

timely :: Int -> App stage () -> App stage ()
timely interval task = do

  initialTime <- liftIO $ timeCurrentP

  void $ flip State.runStateT initialTime $ forever $ do

    lastTime <- State.get

    let nextTime =
          addElapsedP lastTime $
          ElapsedP 0 $ NanoSeconds $
          fromIntegral interval * 1000

    State.put nextTime

    lift $ do
      task

      now <- liftIO timeCurrentP

      let delay =
            case timeDiffP nextTime now of
              (Seconds sec, NanoSeconds nsec) ->
                sec * 1000000 + nsec `div` 1000

      liftIO $ threadDelay $ fromIntegral delay
