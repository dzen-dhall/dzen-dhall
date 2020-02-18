{-# LANGUAGE TemplateHaskell #-}
module DzenDhall.Runtime.Data where

import           DzenDhall.Arguments
import           DzenDhall.Data
import           DzenDhall.Config hiding (Hook)

import           Control.Concurrent.MVar
import           Data.IORef
import           Dhall hiding (maybe)
import           Lens.Micro.TH
import           System.IO
import           System.Process (ProcessHandle)
import qualified Data.HashMap.Strict as H

apiVersion :: Int
apiVersion = 1

data Runtime = Runtime
  { _rtConfigDir :: String
  , _rtConfigurations :: [Configuration]
  , _rtDzenBinary :: String
  , _rtArguments :: Arguments
  , _rtSupportsANSI :: Bool
  , _rtExitMVar :: MVar ()
  }
  deriving (Eq)

makeLenses ''Runtime

-- | 'StateTransitionTable' is needed to know *how* to update,
-- @'IORef' ('Bar' 'Initialized')@ is needed to know *what* to update.
data Subscription
  = AutomatonSubscription
    AutomatonAddress
    StateTransitionTable
    (H.HashMap AutomatonState (Bar Initialized))
    (IORef AutomatonState)
    (IORef (Bar Initialized))

type Subscriptions = H.HashMap Scope [Subscription]

-- | A mapping from clickable area identifiers to script contents.
-- We maintain this mapping to allow using scripts containing `)` in @^ca@.
-- @dzen2@ doesn't allow this.
type ClickableAreas = H.HashMap Int Text

type AutomataCache
  = H.HashMap (Scope, AutomatonAddress)
    ( IORef (Bar Initialized)
    , StateTransitionTable
    , H.HashMap AutomatonState (Bar Marshalled)
    )

data StartupState
  = StartupState
  { _ssSubscriptions       :: Subscriptions
  , _ssScopeName           :: Scope
  , _ssBarSettings         :: BarSettings
  , _ssNonce               :: Int
  -- ^ Counter that is incremented each time it is requested (used as a source
  -- of unique identifiers). See also: 'DzenDhall.App.getCounter'
  , _ssSourceCache         :: H.HashMap (Text, Source) (IORef Text, Cache)
  , _ssAutomataCache       :: AutomataCache
  , _ssSourceQueue         :: [(Source, IORef Text, Cache, Text)]
  , _ssVariableDefinitions :: [(Scope, VariableName, Value)]
  , _ssImages              :: H.HashMap ImageContents ImageId
  -- ^ A queue containing ready-to-be-initialized `Source`s and their handles &
  -- scope names.
  -- This queue is needed because we want to create a `BarRuntime` before
  -- actually running the source processes (they depend on `brEmitterScript` value).
  , _ssClickableAreas      :: ClickableAreas
  -- ^ A mapping from clickable area identifiers to scripts
  , _ssNamedPipe           :: String
  , _ssEmitterFile         :: String
  , _ssGetterFile          :: String
  , _ssSetterFile          :: String
  , _ssVariableFilePrefix  :: String
  , _ssImagePathPrefix     :: String
  }

makeLenses ''StartupState

data BarRuntime = BarRuntime
  { _brConfiguration :: Configuration
  , _brFrameCounter :: Int
  , _brNamedPipe :: String
  -- ^ Named pipe to use as a communication channel for listening to mouse events
  , _brEmitterScript :: String
  -- ^ A script that can be used to emit events
  , _brGetterScript :: String
  , _brSetterScript :: String
  , _brHandle :: Handle
  -- ^ A handle to write to. The value is either stdin of a @dzen2@ process or
  -- 'System.IO.stdout', if @--stdout@ flag is passed.
  , _brDzenHandle :: Maybe ProcessHandle
  }

makeLenses ''BarRuntime
