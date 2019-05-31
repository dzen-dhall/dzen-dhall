{-# LANGUAGE QuasiQuotes #-}
module Main where

import Paths_dzen_dhall
import System.FilePath ((</>))
import qualified GHC.IO.Encoding
import qualified System.IO
import Test.Tasty
import DzenDhall.Test.Config
import DzenDhall.Test.Parser

main :: IO ()
main = do
  GHC.IO.Encoding.setLocaleEncoding System.IO.utf8

  dhallDir <- (</> "dhall") <$> getDataDir

  allTests <- testGroup "DzenDhall" <$> sequence
              [ DzenDhall.Test.Config.getTests dhallDir
              , DzenDhall.Test.Parser.getTests
              ]

  Test.Tasty.defaultMain allTests