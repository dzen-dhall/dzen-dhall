module DzenDhall.Extra where

import           Control.Monad
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Except
import qualified Data.List
import qualified Data.Text
import           Data.Text (Text)

nonNegative :: (Num a, Ord a) => a -> a
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

withMaybe :: Maybe a -> b -> (a -> b) -> b
withMaybe mb b f = maybe b f mb

withEither :: Either a b -> (a -> c) -> (b -> c) -> c
withEither ei l r = either l r ei

safeHead :: [a] -> Maybe a
safeHead = fmap fst . Data.List.uncons

safeTail :: [a] -> Maybe [a]
safeTail = fmap snd . Data.List.uncons

fromLines :: [Text] -> Text
fromLines = Data.Text.intercalate "\n"

hush :: Either e a -> Maybe a
hush (Left _) = Nothing
hush (Right a) = Just a

throwMaybe :: Monad m => MaybeT m a
throwMaybe = exceptToMaybeT (throwE ())
