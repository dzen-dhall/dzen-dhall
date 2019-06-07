{-# LANGUAGE RankNTypes #-}
{-# OPTIONS -Wno-name-shadowing #-}
module DzenDhall.Parser where

import Data.Text
import DzenDhall.Config
import DzenDhall.Data
import Text.Parsec.Combinator
import Text.Parsec.Prim

type Parser a = Parsec Tokens () a
type Tokens = [DzenDhall.Config.Token]
type Preview t a = forall m u . Monad m => ParsecT t u m a

bar :: Parser (Bar SourceSettings)
bar = topLevel <* eof
  where
    topLevel = fmap Bars $ many $
          Raw    <$> raw
      <|> Txt    <$> txt
      <|> Source <$> source
      <|> wrapped
    wrapped = do
      tag      <- opening
      children <- topLevel
      closing
      pure $ case tag of
               OMarquee speed -> Marquee speed children
               OColor   color -> Color color children


opening :: Preview Tokens OpeningTag
opening = withPreview $ \case
  TokOpen tag -> Just tag
  _           -> Nothing

closing :: Preview Tokens ()
closing = withPreview $ \case
  TokClose -> Just ()
  _        -> Nothing

raw :: Preview Tokens Text
raw = withPreview $ \case
  TokRaw txt -> Just txt
  _          -> Nothing

txt :: Preview Tokens Text
txt = withPreview $ \case
  TokTxt txt -> Just txt
  _          -> Nothing

source :: Preview Tokens SourceSettings
source = withPreview $ \case
  TokSource settings -> Just settings
  _                  -> Nothing

withPreview :: Stream s m t => Show t => (t -> Maybe a) -> ParsecT s u m a
withPreview = tokenPrim show (\pos _ _ -> pos)
