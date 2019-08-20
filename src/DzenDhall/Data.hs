{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module DzenDhall.Data where

import           Data.IORef
import           Data.Text (Text)
import           Data.Vector
import           Data.Void
import           DzenDhall.Config
import           GHC.Generics
import           Lens.Micro.TH
import qualified Data.HashMap.Strict as H

type Cache = IORef (Maybe Text)

data SourceHandle
  = SourceHandle
  { _shOutputRef :: IORef Text
  , _shCacheRef :: Cache
  , _shEscapeMode :: EscapeMode
  }

makeLenses ''SourceHandle

data Bar id
  = BarAutomaton Text (StateTransitionTableX id) (AutomataRefX id (Bar id))
  | BarPad Int Padding (Bar id)
  | BarTrim Int Direction (Bar id)
  | BarListener Text (Bar id)
  | BarMarquee Marquee (Bar id)
  | BarProp Property (Bar id)
  | BarMarkup Text
  | BarScope (Bar id)
  | BarShape Shape
  | BarSlider Slider (Vector (Bar id))
  | BarSource (SourceRefX id)
  | BarText Text
  | BarDefine Variable
  | Bars [Bar id]
  deriving (Generic)

type family AutomataRefX          id :: * -> *
type family StateTransitionTableX id
type family SourceRefX            id

newtype Initialized = Initialized Void
newtype Marshalled  = Marshalled  Void

type instance AutomataRefX          Marshalled = H.HashMap Text
type instance StateTransitionTableX Marshalled = StateTransitionTable
type instance SourceRefX            Marshalled = Source

type instance AutomataRefX          Initialized = IORef
type instance StateTransitionTableX Initialized = ()
type instance SourceRefX            Initialized = SourceHandle

deriving instance Show (Bar Marshalled)
deriving instance Eq   (Bar Marshalled)

instance Semigroup (Bar id) where
  a <> b = Bars [a, b]

instance Monoid (Bar id) where
  mempty = Bars []

data Property
  = BG Color
  | IB
  | FG Color
  | CA ClickableArea
  | P Position
  | PA AbsolutePosition
  deriving (Eq, Show, Generic)

type Handler = Text
type ImagePath = Text

data Shape
  = I ImagePath
  | R Int Int
  | RO Int Int
  | C Int
  | CO Int
  deriving (Eq, Show, Generic)
