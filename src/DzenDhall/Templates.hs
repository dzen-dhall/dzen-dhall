{-# LANGUAGE TemplateHaskell #-}
module DzenDhall.Templates where

import FileEmbedLzma
import Data.ByteString

staticFiles :: [(FilePath, ByteString)]
staticFiles = $(embedRecursiveDir "dhall")
