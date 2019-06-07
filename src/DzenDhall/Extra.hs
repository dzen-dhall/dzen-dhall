module DzenDhall.Extra where

import Data.Text
import Control.Monad

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
