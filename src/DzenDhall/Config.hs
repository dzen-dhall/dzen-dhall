module DzenDhall.Config where

import Dhall
import Dhall.Core
import Data.Text (Text)
import Data.Functor
import Data.Time.Clock.POSIX

data OpeningTag
  = OMarquee Integer
  | OColor Text
  deriving (Show, Eq, Generic)

openingTag :: Type OpeningTag
openingTag = union
  $  (OMarquee <$> constructor "Marquee" integer)
  <> (OColor   <$> constructor "Color" strictText)

data BarSettings = BarSettings
  deriving (Show, Eq, Generic)

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

type Bar = [Token]

data Config = Config
  { bar :: Bar
  , settings :: BarSettings
  }
  deriving (Show, Eq, Generic)
