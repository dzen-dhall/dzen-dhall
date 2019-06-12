{-# LANGUAGE TemplateHaskell #-}
module DzenDhall.Config where

import           Data.Coerce
import qualified Data.HashMap.Strict as H
import           Data.Hashable
import           Data.Text (Text)
import           Dhall
import           Lens.Micro.TH

import           DzenDhall.Extra


data Marquee
  = Marquee
  { _mqFramesPerChar :: Int
  , _mqWidth         :: Int
  }
  deriving (Show, Eq, Generic)

makeLenses ''Marquee

marqueeType :: Type Marquee
marqueeType = record $
  Marquee <$> field "framesPerCharacter" (positive    . fromIntegral <$> natural)
          <*> field "width"              (nonNegative . fromIntegral <$> natural)

data VDirection
  = VUp | VDown
  deriving (Show, Eq, Generic)

directionType :: Type VDirection
directionType = union
  $  (VUp   <$ constructor "Up"   unit)
  <> (VDown <$ constructor "Down" unit)

data MouseButton
  = MouseLeft
  | MouseMiddle
  | MouseRight
  | MouseScrollUp
  | MouseScrollDown
  | MouseScrollLeft
  | MouseScrollRight
  deriving (Show, Eq, Generic)

instance Hashable MouseButton

mouseButtonType :: Type MouseButton
mouseButtonType = union
  $  (MouseLeft        <$ constructor "Left"        unit)
  <> (MouseMiddle      <$ constructor "Middle"      unit)
  <> (MouseRight       <$ constructor "Right"       unit)
  <> (MouseScrollUp    <$ constructor "ScrollUp"    unit)
  <> (MouseScrollDown  <$ constructor "ScrollDown"  unit)
  <> (MouseScrollLeft  <$ constructor "ScrollLeft"  unit)
  <> (MouseScrollRight <$ constructor "ScrollRight" unit)

data Fade
  = Fade
  { _fadeDirection   :: VDirection
  , _fadeFrameCount  :: Int
  , _fadePixelHeight :: Int
  }
  deriving (Show, Eq, Generic)

makeLenses ''Fade

fadeType :: Type Fade
fadeType = record $
  Fade <$> field "direction"  directionType
       <*> field "frameCount" (fromIntegral <$> natural)
       <*> field "height"     (fromIntegral <$> natural)

data Slider
  = Slider
  { _fadeIn      :: Fade
  , _fadeOut     :: Fade
  , _sliderDelay :: Int

  }
  deriving (Show, Eq, Generic)

makeLenses ''Slider

sliderType :: Type Slider
sliderType = record $
  Slider <$> field "fadeIn"  fadeType
         <*> field "fadeOut" fadeType
         <*> field "delay"   (fromIntegral <$> natural)

newtype StateTransitionTable = StateTransitionTable { unSTT :: H.HashMap (MouseButton, Text) Text }
  deriving (Show, Eq, Generic)

stateTransitionTableType :: Type StateTransitionTable
stateTransitionTableType = coerce <$> H.fromList <$>
  list (record $
        ((\e f t -> ((e, f), t))
          <$> field "event" mouseButtonType
          <*> field "from"  strictText
          <*> field "to"    strictText))

data OpeningTag
  = OMarquee Marquee
  | OSlider  Slider
  | OColor   Text
  | OAutomaton StateTransitionTable
  | OStateMapKey Text
  deriving (Show, Eq, Generic)

openingTagType :: Type OpeningTag
openingTagType = union
  $  (OMarquee     <$> constructor "Marquee"     marqueeType)
  <> (OSlider      <$> constructor "Slider"      sliderType)
  <> (OColor       <$> constructor "Color"       strictText)
  <> (OAutomaton   <$> constructor "Automaton"   stateTransitionTableType)
  <> (OStateMapKey <$> constructor "StateMapKey" strictText)

data BarSettings
  = BarSettings
  { _bsMonitor :: Int
  -- ^ Xinerama monitor number
  , _bsExtraFlags     :: [String]
  -- ^ Extra flags to pass to dzen binary
  , _bsUpdateInterval :: Int
  -- ^ In microseconds
  , _bsFont           :: Maybe String
  -- ^ Font in XLFD format
  , _bsFontWidth      :: Maybe Int
  }
  deriving (Show, Eq, Generic)

makeLenses ''BarSettings

barSettingsType :: Type BarSettings
barSettingsType = record $
  BarSettings <$> field "monitor"        (fromIntegral <$> natural)
              <*> field "extraFlags"     (list string)
              <*> field "updateInterval" ((* 1000) . fromIntegral <$> natural)
              <*> field "font"           (Dhall.maybe string)
              <*> field "fontWidth"      (fmap fromIntegral <$> Dhall.maybe natural)

data Token
  = TokOpen OpeningTag
  | TokRaw Text
  | TokSource Source
  | TokTxt Text
  | TokSeparator
  | TokClose
  deriving (Show, Eq, Generic)

tokenType :: Type Token
tokenType = union
  $  (TokOpen      <$> constructor "Open"      openingTagType)
  <> (TokRaw       <$> constructor "Raw"       strictText)
  <> (TokSource    <$> constructor "Source"    sourceSettingsType)
  <> (TokTxt       <$> constructor "Txt"       strictText)
  <> (TokSeparator <$  constructor "Separator" unit)
  <> (TokClose     <$  constructor "Close"     unit)

stateMapType :: Type (H.HashMap Text [Token])
stateMapType = H.fromList <$>
  list (record $
         (,) <$> field "state" strictText
             <*> field "bar"   (list tokenType))

data EscapeMode = EscapeMode
  { joinLines    :: Bool
  , escapeMarkup :: Bool
  }
  deriving (Show, Eq, Generic)

escapeModeType :: Type EscapeMode
escapeModeType = record $
  EscapeMode <$> field "joinLines"    bool
             <*> field "escapeMarkup" bool

data Source
  = Source
  { updateInterval :: Maybe Int
  -- ^ In microseconds
  , command        :: [String]
  , stdin          :: Maybe Text
  , escapeMode     :: EscapeMode
  } deriving (Show, Eq, Generic)

sourceSettingsType :: Type Source
sourceSettingsType = record $
  Source <$> field "updateInterval" (Dhall.maybe $ (* 1000) . fromIntegral <$> natural)
         <*> field "command"        (list string)
         <*> field "stdin"          (Dhall.maybe strictText)
         <*> field "escapeMode"     escapeModeType

data Configuration = Configuration
  { _cfgBarTokens   :: [Token]
  , _cfgBarSettings :: BarSettings
  }
  deriving (Show, Eq, Generic)

makeLenses ''Configuration

configurationType :: Type Configuration
configurationType = record $
  Configuration <$> field "bar"      (list tokenType)
                <*> field "settings" barSettingsType

data PluginMeta = PluginMeta
  { _pmName             :: Text
  , _pmAuthor           :: Text
  , _pmEmail            :: Maybe Text
  , _pmHomePage         :: Maybe Text
  , _pmUpstreamURL      :: Maybe Text
  , _pmDescription      :: Text
  , _pmUsage            :: Text
  , _pmRequiredBinaries :: [Text]
  , _pmApiVersion       :: Int
  }
  deriving (Show, Eq)

makeLenses ''PluginMeta

pluginMetaType :: Type PluginMeta
pluginMetaType = record $
  PluginMeta <$> field "name"             strictText
             <*> field "author"           strictText
             <*> field "email"            (Dhall.maybe strictText)
             <*> field "homepage"         (Dhall.maybe strictText)
             <*> field "upstreamURL"      (Dhall.maybe strictText)
             <*> field "description"      strictText
             <*> field "usage"            strictText
             <*> field "requiredBinaries" (list strictText)
             <*> field "apiVersion"       (fromIntegral <$> natural)
