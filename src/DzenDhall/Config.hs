{-# LANGUAGE TemplateHaskell #-}
module DzenDhall.Config where

import Dhall
import Data.Text (Text)
import Lens.Micro.TH
import DzenDhall.Extra

data MarqueeSettings
  = MarqueeSettings
  { _mqFramesPerChar :: Int
  , _mqWidth :: Int
  }
  deriving (Show, Eq, Generic)

makeLenses ''MarqueeSettings

marqueeSettingsType :: Type MarqueeSettings
marqueeSettingsType = record $
  MarqueeSettings <$> field "framesPerCharacter" (positive    . fromIntegral <$> natural)
                  <*> field "width"              (nonNegative . fromIntegral <$> natural)

data OpeningTag
  = OMarquee MarqueeSettings
  | OColor Text
  deriving (Show, Eq, Generic)

openingTagType :: Type OpeningTag
openingTagType = union
  $  (OMarquee <$> constructor "Marquee" marqueeSettingsType)
  <> (OColor   <$> constructor "Color"   strictText)

data BarSettings
  = BarSettings
  { _bsMonitor :: Int
  -- ^ Xinerama monitor number
  , _bsExtraFlags :: [String]
  -- ^ Extra flags to pass to dzen binary
  , _bsUpdateInterval :: Int
  -- ^ In microseconds
  , _bsFont :: Maybe String
  -- ^ Font in XLFD format
  , _bsFontWidth :: Maybe Int
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
  | TokSource SourceSettings
  | TokTxt Text
  | TokClose
  deriving (Show, Eq, Generic)

tokenType :: Type Token
tokenType = union
  $  (TokOpen   <$> constructor "Open"   openingTagType)
  <> (TokRaw    <$> constructor "Raw"    strictText)
  <> (TokSource <$> constructor "Source" sourceSettingsType)
  <> (TokTxt    <$> constructor "Txt"    strictText)
  <> (TokClose  <$  constructor "Close"  unit)

data EscapeMode = EscapeMode
  { joinLines :: Bool
  , escapeMarkup :: Bool
  }
  deriving (Show, Eq, Generic)

escapeModeType :: Type EscapeMode
escapeModeType = record $
  EscapeMode <$> field "joinLines" bool
             <*> field "escapeMarkup" bool

data SourceSettings
  = SourceSettings
  { updateInterval :: Maybe Int
  -- ^ In microseconds
  , command :: [String]
  , stdin :: Maybe Text
  , escapeMode :: EscapeMode
  } deriving (Show, Eq, Generic)

sourceSettingsType :: Type SourceSettings
sourceSettingsType = record $
  SourceSettings <$> field "updateInterval" (Dhall.maybe $ (* 1000) . fromIntegral <$> natural)
                 <*> field "command"        (list string)
                 <*> field "stdin"          (Dhall.maybe strictText)
                 <*> field "escapeMode"     escapeModeType

type BarSpec = [Token]

data Configuration = Configuration
  { _cfgBarSpec :: BarSpec
  , _cfgBarSettings :: BarSettings
  }
  deriving (Show, Eq, Generic)

makeLenses ''Configuration

configurationType :: Type Configuration
configurationType = record $
  Configuration <$> field "bar" (list tokenType)
                <*> field "settings" barSettingsType
