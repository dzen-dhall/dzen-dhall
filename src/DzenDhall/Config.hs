module DzenDhall.Config where

import Dhall
import Data.Text (Text)

data OpeningTag
  = OMarquee Integer
  | OColor Text
  deriving (Show, Eq, Generic)

openingTag :: Type OpeningTag
openingTag = union
  $  (OMarquee <$> constructor "Marquee" integer)
  <> (OColor   <$> constructor "Color" strictText)

data BarSettings
  = BarSettings
  { monitor :: Int
  , extraFlags :: [String]
  }
  deriving (Show, Eq, Generic)

barSettings :: Type BarSettings
barSettings = record $
  BarSettings <$> field "monitor"    (fromIntegral <$> natural)
              <*> field "extraFlags" (list string)

data Token
  = TokOpen OpeningTag
  | TokRaw Text
  | TokSource SourceSettings
  | TokTxt Text
  | TokClose
  deriving (Show, Eq, Generic)

token :: Type Token
token = union
  $  (TokOpen   <$> constructor "Open"  openingTag)
  <> (TokRaw    <$> constructor "Raw"   strictText)
  <> (TokSource <$> constructor "Source" sourceSettings)
  <> (TokTxt    <$> constructor "Txt"   strictText)
  <> (TokClose  <$  constructor "Close" unit)

data SourceSettings
  = SourceSettings
  { updateInterval :: Maybe Int
  , command :: [String]
  , stdin :: Maybe Text
  } deriving (Show, Eq, Generic)

sourceSettings :: Type SourceSettings
sourceSettings = record $
  SourceSettings <$> field "updateInterval" (Dhall.maybe $ fromIntegral <$> natural)
                 <*> field "command"        (list string)
                 <*> field "stdin"          (Dhall.maybe strictText)

data MarqueeSettings
  = MarqueeSettings
  { marqueeSpeed :: Int
  , marqueeFramesPerChar :: Int
  , marqueeWidth :: Int
  }
  deriving (Show, Eq, Generic)

marqueeSettings :: Type MarqueeSettings
marqueeSettings = record $
  MarqueeSettings <$> field "speed"              (fromIntegral <$> integer)
                  <*> field "framesPerCharacter" (fromIntegral <$> natural)
                  <*> field "width"              (fromIntegral <$> natural)

type BarSpec = [Token]

data Configuration = Configuration
  { bar :: BarSpec
  , settings :: BarSettings
  }
  deriving (Show, Eq, Generic)

configuration :: Type Configuration
configuration = record $
  Configuration <$> field "bar" (list token)
                <*> field "settings" barSettings
