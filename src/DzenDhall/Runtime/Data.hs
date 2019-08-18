{-# LANGUAGE TemplateHaskell #-}
module DzenDhall.Runtime.Data where

import           DzenDhall.Arguments
import           DzenDhall.Data
import           DzenDhall.Config hiding (Hook)

import           Data.IORef
import           Dhall hiding (maybe)
import           Lens.Micro.TH
import           System.IO
import qualified Data.HashMap.Strict as H

apiVersion :: Int
apiVersion = 1

data Runtime = Runtime
  { _rtConfigDir :: String
  , _rtConfigurations :: [Configuration]
  , _rtDzenBinary :: String
  , _rtAPIVersion :: Int
  , _rtArguments :: Arguments
  }
  deriving (Eq, Show)

makeLenses ''Runtime

type AutomatonState = Text
type Slot = Text
type Scope = Text

-- | 'StateTransitionTable' is needed to know *how* to update,
-- @'IORef' ('Bar' 'Initialized')@ is needed to know *what* to update.
data Subscription
  = AutomatonSubscription
    StateTransitionTable
    (H.HashMap AutomatonState (Bar Initialized))
    (IORef AutomatonState)
    (IORef (Bar Initialized))

type AutomataHandles = H.HashMap (Slot, Scope) [Subscription]

-- | A mapping from clickable area identifiers to script contents.
-- We maintain this mapping to allow using scripts containing `)` in @^ca@.
-- @dzen2@ doesn't allow this.
type ClickableAreas = H.HashMap Int Text

data StartupState
  = StartupState
  { _ssAutomataHandles :: AutomataHandles
  , _ssScopeName :: Text
  , _ssBarSettings :: BarSettings
  , _ssCounter :: Int
  -- ^ Counter that is incremented each time it is requested (used as a source
  -- of unique identifiers). See also: 'DzenDhall.App.getCounter'
  , _ssSourceCache :: H.HashMap (Text, Source) (IORef Text, Cache)
  , _ssAutomataCache :: H.HashMap (Text, Text) (IORef (Bar Initialized))
  , _ssSourceQueue :: [(Source, IORef Text, Cache, Text)]
  -- ^ A queue containing ready-to-be-initialized `Source`s and their handles &
  -- scope names.
  -- This queue is needed because we want to create a `BarRuntime` before
  -- actually running the source processes (they depend on `brEmitterScript` value).
  , _ssClickableAreas :: ClickableAreas
  -- ^ A mapping from clickable area identifiers to scripts
  , _ssNamedPipe :: String
  , _ssEmitterFile :: String
  }

makeLenses ''StartupState

data BarRuntime = BarRuntime
  { _brConfiguration :: Configuration
  , _brFrameCounter :: Int
  , _brNamedPipe :: String
  -- ^ Named pipe to use as a communication channel for listening to mouse events
  , _brEmitterScript :: String
  -- ^ A script that can be used to emit events
  , _brHandle :: Handle
  -- ^ A handle to write to. The value is either stdin of a @dzen2@ process or
  -- 'System.IO.stdout', if @--stdout@ flag is passed.
  }
  deriving (Eq, Show)

makeLenses ''BarRuntime
