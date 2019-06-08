{-# OPTIONS -Wno-name-shadowing #-}
module DzenDhall.Parser where

import Data.Text
import DzenDhall.Config
import DzenDhall.Data
import Text.Parsec.Combinator
import Text.Parsec.Prim
import Text.Parsec

type Parser a = Parsec Tokens () a

type Tokens = [DzenDhall.Config.Token]

-- | Used to tag bar elements that require a list of children.
-- These lists of children are parsed using 'sepBy' with 'TokSeparator' as a delimiter.
data Separatable = SepSlider Slider

-- | Used to tag bar elements that require a single child.
data Solid = SolidMarquee Marquee | SolidColor Color

bar :: Parser (Bar Source)
bar = topLevel <* eof
  where
    topLevel = fmap Bars $ many $
          BarRaw    <$> raw
      <|> BarText   <$> text
      <|> BarSource <$> source
      <|> separated
      <|> wrapped

    separated = do
      tag <- separatable
      children <- topLevel `sepBy` separator
      closing
      pure $
        case tag of
          SepSlider slider -> BarSlider slider children

    wrapped = do
      tag      <- solid
      children <- topLevel
      closing
      pure $
        case tag of
          SolidMarquee settings -> BarMarquee settings children
          SolidColor   color    -> BarColor   color    children

separatable :: Parser Separatable
separatable = withPreview $ \case
  TokOpen (OSlider slider) -> Just (SepSlider slider)
  _                        -> Nothing

separator :: Parser ()
separator = withPreview $ \case
  TokSeparator -> Just ()
  _            -> Nothing

solid :: Parser Solid
solid = withPreview $ \case
  TokOpen (OMarquee marquee) -> Just $ SolidMarquee marquee
  TokOpen (OColor   color)   -> Just $ SolidColor color
  _                          -> Nothing

closing :: Parser ()
closing = withPreview $ \case
  TokClose -> Just ()
  _        -> Nothing

raw :: Parser Text
raw = withPreview $ \case
  TokRaw txt -> Just txt
  _          -> Nothing

text :: Parser Text
text = withPreview $ \case
  TokTxt txt -> Just txt
  _          -> Nothing

source :: Parser Source
source = withPreview $ \case
  TokSource settings -> Just settings
  _                  -> Nothing

withPreview :: Stream s m t => Show t => (t -> Maybe a) -> ParsecT s u m a
withPreview = tokenPrim show (\pos _ _ -> pos)

runBarParser :: Tokens -> Either ParseError (Bar Source)
runBarParser = Text.Parsec.runParser bar () "Bar"
