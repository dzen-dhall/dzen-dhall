{-# LANGUAGE RankNTypes #-}
module DzenDhall.Parse where

import DzenDhall.Config
import DzenDhall.Tree
import Text.Parsec.Prim
import Text.Parsec.Combinator
import Data.Text

type Parser a = Parsec [DzenDhall.Config.Token] () a
type Tokens = [DzenDhall.Config.Token]
type Preview t a = forall m u. Monad m => ParsecT t u m a

satisfy :: Stream s m Plugin => (Plugin -> Bool) -> ParsecT s u m Plugin
satisfy p =
  tokenPrim
  show
  (\pos _ _ -> pos)
  (\c -> if p c then Just c else Nothing)

opening :: Monad m => ParsecT Tokens u m OpeningTag
opening = withPreview
          (\case
              TokOpen tag -> Just tag
              _ -> Nothing)


closing :: Preview Tokens () -- Monad m => ParsecT Tokens u m ()
closing = withPreview
          (\case
              TokClose -> Just ()
              _ -> Nothing)


withPreview :: Stream s m t
     => Show t
     => (t -> Maybe a)
     -> ParsecT s u m a
withPreview = tokenPrim show (\pos _ _ -> pos)

raw :: Preview Tokens Text
raw = withPreview
      (\case
          TokRaw txt -> Just txt
          _ -> Nothing)

txt :: Preview Tokens Text
txt = withPreview
      (\case
          TokTxt txt -> Just txt
          _ -> Nothing)

shell :: Preview Tokens Text
shell = withPreview
      (\case
          TokShell txt -> Just txt
          _ -> Nothing)

plugin :: Parser Plugin
plugin = do
  res <- plugin'
  eof
  pure res
  where
    plugin' = do
      tag <- opening
      children <- fmap Plugins $ many
        $   Raw   <$> raw
        <|> Txt   <$> txt
        <|> Shell <$> shell
        <|> plugin
      closing
      pure $ case tag of
        OMarquee speed -> Marquee speed children
        OColor color -> Color color children
