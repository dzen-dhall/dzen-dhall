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
  | TokShell Text
  | TokTxt Text
  | TokClose
  deriving (Show, Eq, Generic)

token :: Type Token
token = union
  $  (TokOpen  <$> constructor "Open"  openingTag)
  <> (TokRaw   <$> constructor "Raw"   strictText)
  <> (TokShell <$> constructor "Shell" strictText)
  <> (TokTxt   <$> constructor "Txt"   strictText)
  <> (TokClose <$  constructor "Close" unit)

type Bar = [Token]

data Config = Config
  { bar :: Bar
  , settings :: BarSettings
  }
  deriving (Show, Eq, Generic)
