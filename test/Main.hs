{-# LANGUAGE QuasiQuotes #-}
module Main where

import Paths_dzen_dhall
import System.FilePath ((</>))
import qualified GHC.IO.Encoding
import qualified System.IO
import Test.Tasty
import qualified DzenDhall.Test.Config
import qualified DzenDhall.Test.Parser
import qualified DzenDhall.Test.AST
import qualified DzenDhall.Test.Arguments

main :: IO ()
main = do
  GHC.IO.Encoding.setLocaleEncoding System.IO.utf8

  dhallDir <- (</> "dhall") <$> getDataDir

  allTests <- testGroup "DzenDhall" <$> sequence
              [ DzenDhall.Test.Config.getTests dhallDir
              , DzenDhall.Test.Parser.getTests
              , DzenDhall.Test.AST.getTests
              , DzenDhall.Test.Arguments.getTests
              ]

  Test.Tasty.defaultMain allTests
