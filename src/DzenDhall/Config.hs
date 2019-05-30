{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module DzenDhall.Config where

import Dhall
import Dhall.Core
import Data.Text

data OpeningTag
  = OMarquee Integer
  | OColor Text
  deriving (Show, Eq, Generic)

openingTag :: Type OpeningTag
openingTag = union
  $  (OMarquee <$> constructor "Marquee" integer)
  <> (OColor   <$> constructor "Color" strictText)

data ClosingTag
  = CMarquee
  | CColor
  deriving (Show, Eq, Generic)

closingTag :: Type ClosingTag
closingTag = union
  $  (CMarquee <$ constructor "Marquee" unit)
  <> (CColor   <$ constructor "Color"   unit)

data BarSettings = BarSettings
  deriving (Show, Eq, Generic)

data Token
  = Open OpeningTag
  | Raw Text
  | Shell Text
  | Txt Text
  | Close ClosingTag
  deriving (Show, Eq, Generic)

token :: Type Token
token = union
  $  (Open  <$> constructor "Open"  openingTag)
  <> (Raw   <$> constructor "Raw"   strictText)
  <> (Shell <$> constructor "Shell" strictText)
  <> (Txt   <$> constructor "Txt"   strictText)
  <> (Close <$> constructor "Close" closingTag)

type Bar = [Token]

data Config = Config
  { bar :: Bar
  , settings :: BarSettings
  }
