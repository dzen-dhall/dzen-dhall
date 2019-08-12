{-# OPTIONS -Wno-name-shadowing #-}
module DzenDhall.Parser where

import Data.Text
import DzenDhall.Config
import DzenDhall.Data
import Text.Parsec.Combinator
import Text.Parsec.Prim
import Text.Parsec
import qualified Data.HashMap.Strict as H
import qualified Data.Vector as V

type Parser a = Parsec Tokens () a

type Tokens = [DzenDhall.Config.Token]

-- | Used to tag bar elements that require a list of children.
-- These lists of children are parsed using 'sepBy' with 'TokSeparator' as a delimiter.
newtype Separatable = SepSlider Slider

-- | Used to tag bar elements that require a single child.
data Solid
  = SolidMarquee Marquee
  | SolidFG Color
  | SolidBG Color
  | SolidP Position
  | SolidPA AbsolutePosition
  | SolidCA ClickableArea
  | SolidIB
  | SolidListener Text

runBarParser :: Tokens -> Either ParseError (Bar Marshalled)
runBarParser = Text.Parsec.runParser bar () "Bar"

bar :: Parser (Bar Marshalled)
bar = topLevel <* eof

topLevel :: Parser (Bar Marshalled)
topLevel = fmap Bars $ many $
      BarRaw    <$> raw
  <|> BarText   <$> text
  <|> BarSource <$> source
  <|> wrapped
  <|> separated
  <|> automaton

separated :: Parser (Bar Marshalled)
separated = do
  tag <- separatable
  children <- topLevel `sepBy` separator
  closing
  pure $
    case tag of
      SepSlider slider -> BarSlider slider (V.fromList children)

wrapped :: Parser (Bar Marshalled)
wrapped = do
  tag      <- solid
  child <- topLevel
  closing
  pure $
    case tag of
      SolidMarquee settings -> BarMarquee  settings      child
      SolidFG color         -> BarProp     (FG color)    child
      SolidBG color         -> BarProp     (BG color)    child
      SolidP position       -> BarProp     (P position)  child
      SolidPA position      -> BarProp     (PA position) child
      SolidCA ca            -> BarProp     (CA ca)       child
      SolidIB               -> BarProp     IB            child
      SolidListener slot    -> BarListener slot          child

automaton :: Parser (Bar Marshalled)
automaton = do
  (address, stt) <- stateTransitionTable
  kvs <- many $ do
    smk <- stateMapKey
    bar <- topLevel
    closing
    pure (smk, bar)
  closing
  pure $ BarAutomaton address stt $ H.fromList kvs

stateTransitionTable :: Parser (Text, StateTransitionTable)
stateTransitionTable = withPreview $ \case
  TokOpen (OAutomaton address stt) -> Just (address, stt)
  _                                -> Nothing

stateMapKey :: Parser Text
stateMapKey = withPreview $ \case
  TokOpen (OStateMapKey key) -> Just key
  _                          -> Nothing

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
  TokOpen (OFG color)        -> Just $ SolidFG color
  TokOpen (OBG color)        -> Just $ SolidBG color
  TokOpen (OP position)      -> Just $ SolidP position
  TokOpen (OPA position)     -> Just $ SolidPA position
  TokOpen (OCA area)         -> Just $ SolidCA area
  TokOpen OIB                -> Just $ SolidIB
  TokOpen (OListener slot)   -> Just $ SolidListener slot
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
