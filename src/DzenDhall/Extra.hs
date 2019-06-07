module DzenDhall.Extra where

import Data.Text

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
