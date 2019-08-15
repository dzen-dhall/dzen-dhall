module DzenDhall.Extra where

import           Control.Monad
import qualified Data.List
import qualified Data.Text
import           Data.Text (Text)

nonNegative :: Int -> Int
nonNegative x
  | x < 0 = 0
  | otherwise = x

positive :: Int -> Int
positive x
  | x < 1 = 1
  | otherwise = x

spaces :: Int -> Text
spaces w = Data.Text.justifyRight w ' ' ""

showPack :: Show a => a -> Text
showPack = Data.Text.pack . show

loopWhileM :: Monad m => m Bool -> m () -> m ()
loopWhileM pr act = do
    b <- pr
    when b $ do
      act
      loopWhileM pr act

whenJust :: (Monad m, Monoid b) => Maybe a -> (a -> m b) -> m b
whenJust = flip $ maybe (return mempty)

leftToJust :: Either a b -> Maybe a
leftToJust (Left a) = Just a
leftToJust _ = Nothing

safeHead :: [a] -> Maybe a
safeHead = fmap fst . Data.List.uncons

safeTail :: [a] -> Maybe [a]
safeTail = fmap snd . Data.List.uncons

fromLines :: [Text] -> Text
fromLines = Data.Text.intercalate "\n"
