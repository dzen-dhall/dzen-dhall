{-# LANGUAGE RankNTypes #-}
module DzenDhall.Parser where

import Data.Text
import DzenDhall.Config
import DzenDhall.Data
import Text.Parsec.Combinator
import Text.Parsec.Prim

type Parser a = Parsec [DzenDhall.Config.Token] () a
type Tokens = [DzenDhall.Config.Token]
type Preview t a = forall m u . Monad m => ParsecT t u m a

opening :: Preview Tokens OpeningTag
opening = withPreview $ \case
        TokOpen tag -> Just tag
        _           -> Nothing

closing :: Preview Tokens ()
closing = withPreview $ \case
        TokClose -> Just ()
        _        -> Nothing

withPreview :: Stream s m t => Show t => (t -> Maybe a) -> ParsecT s u m a
withPreview = tokenPrim show (\pos _ _ -> pos)

raw :: Preview Tokens Text
raw = withPreview $ \case
        TokRaw txt -> Just txt
        _          -> Nothing

txt :: Preview Tokens Text
txt = withPreview $ \case
        TokTxt txt -> Just txt
        _          -> Nothing

source :: Preview Tokens Text
source = withPreview $ \case
        TokSource txt -> Just txt
        _             -> Nothing

plugin :: Parser (Plugin ())
plugin = plugin' <* eof
  where
    plugin' = do
      tag      <- opening
      children <- fmap Plugins $ many
        $   Raw       <$> raw
        <|> Txt       <$> txt
        <|> Source () <$> source
        <|> plugin'
      closing
      pure $ case tag of
               OMarquee speed -> Marquee speed children
               OColor   color -> Color color children
