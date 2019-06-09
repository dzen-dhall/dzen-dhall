{-# LANGUAGE TemplateHaskell #-}
module DzenDhall.Config where

import Dhall
import Data.Text (Text)
import Lens.Micro.TH
import DzenDhall.Extra

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

data Fade
  = Fade
  { _fadeDirection  :: VDirection
  , _fadeFrameCount :: Int
  }
  deriving (Show, Eq, Generic)

makeLenses ''Fade

fadeType :: Type Fade
fadeType = record $
  Fade <$> field "direction"  directionType
       <*> field "frameCount" (fromIntegral <$> natural)

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

data OpeningTag
  = OMarquee Marquee
  | OSlider  Slider
  | OColor   Text
  deriving (Show, Eq, Generic)

openingTagType :: Type OpeningTag
openingTagType = union
  $  (OMarquee <$> constructor "Marquee" marqueeType)
  <> (OSlider  <$> constructor "Slider"  sliderType)
  <> (OColor   <$> constructor "Color"   strictText)

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

type BarSpec = [Token]

data Configuration = Configuration
  { _cfgBarSpec     :: BarSpec
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
