module DzenDhall.Config where

import Dhall
import Dhall.Core
import Data.Text
import Data.Functor

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
  | TokSource Text
  | TokTxt Text
  | TokClose
  deriving (Show, Eq, Generic)

token :: Type Token
token = union
  $  (TokOpen   <$> constructor "Open"  openingTag)
  <> (TokRaw    <$> constructor "Raw"   strictText)
  <> (TokSource <$> constructor "Shell" strictText)
  <> (TokTxt    <$> constructor "Txt"   strictText)
  <> (TokClose  <$  constructor "Close" unit)

data SourceSettings
  = SourceSettings
  { updateInterval :: Maybe Natural
  , command :: [Text]
  , stdin :: Maybe Text
  } deriving (Show, Eq, Generic)

sourceSettings :: Type SourceSettings
sourceSettings = record
  ( SourceSettings <$> field "updateInterval" (Dhall.maybe natural)
                   <*> field "command"        (list strictText)
                   <*> field "stdin"          (Dhall.maybe strictText)
  )

type Bar = [Token]

data Config = Config
  { bar :: Bar
  , settings :: BarSettings
  }
  deriving (Show, Eq, Generic)
