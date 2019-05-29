module DzenDhall.Runtime where

data Runtime = Runtime
  { configPath :: String
  }
  deriving (Eq, Show)
