module DzenDhall.Config where

import Dhall
import Data.Text (Text)

data OpeningTag
  = OMarquee Integer
  | OColor Text
  deriving (Show, Eq, Generic)

openingTagType :: Type OpeningTag
openingTagType = union
  $  (OMarquee <$> constructor "Marquee" integer)
  <> (OColor   <$> constructor "Color" strictText)

data BarSettings
  = BarSettings
  { bsMonitor :: Int
  -- ^ Xinerama monitor number
  , bsExtraFlags :: [String]
  -- ^ Extra flags to pass to dzen binary
  , bsUpdateInterval :: Int
  -- ^ in microseconds
  }
  deriving (Show, Eq, Generic)

barSettingsType :: Type BarSettings
barSettingsType = record $
  BarSettings <$> field "monitor"        (fromIntegral <$> natural)
              <*> field "extraFlags"     (list string)
              <*> field "updateInterval" ((* 1000) . fromIntegral <$> natural)

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
  , command :: [String]
  , stdin :: Maybe Text
  , escapeMode :: EscapeMode
  } deriving (Show, Eq, Generic)

sourceSettingsType :: Type SourceSettings
sourceSettingsType = record $
  SourceSettings <$> field "updateInterval" (Dhall.maybe $ fromIntegral <$> natural)
                 <*> field "command"        (list string)
                 <*> field "stdin"          (Dhall.maybe strictText)
                 <*> field "escapeMode"     escapeModeType

data MarqueeSettings
  = MarqueeSettings
  { mqSpeed :: Int
  , mqFramesPerChar :: Int
  , mqWidth :: Int
  }
  deriving (Show, Eq, Generic)

marqueeSettingsType :: Type MarqueeSettings
marqueeSettingsType = record $
  MarqueeSettings <$> field "speed"              (fromIntegral <$> integer)
                  <*> field "framesPerCharacter" (fromIntegral <$> natural)
                  <*> field "width"              (fromIntegral <$> natural)

type BarSpec = [Token]

data Configuration = Configuration
  { bar :: BarSpec
  , settings :: BarSettings
  }
  deriving (Show, Eq, Generic)

configurationType :: Type Configuration
configurationType = record $
  Configuration <$> field "bar" (list tokenType)
                <*> field "settings" barSettingsType
