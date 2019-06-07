module DzenDhall.Extra where

import Data.Text

nonNegative :: Int -> Int
nonNegative x
  | x < 0 = 0
  | otherwise = x

spaces :: Int -> Text
spaces w = Data.Text.justifyRight w ' ' ""
