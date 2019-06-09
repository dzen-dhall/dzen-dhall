{-# LANGUAGE QuasiQuotes #-}
module Main where

import qualified DzenDhall.Test.AST
import qualified DzenDhall.Test.Animation.Marquee
import qualified DzenDhall.Test.Arguments
import qualified DzenDhall.Test.Config
import qualified DzenDhall.Test.Parser
import qualified DzenDhall.Test.Plug
import qualified GHC.IO.Encoding
import           Paths_dzen_dhall
import           System.FilePath ((</>))
import qualified System.IO
import           Test.Tasty

main :: IO ()
main = do
  GHC.IO.Encoding.setLocaleEncoding System.IO.utf8

  dhallDir <- (</> "dhall") <$> getDataDir

  allTests <- testGroup "DzenDhall" <$> sequence
              [ DzenDhall.Test.Config.getTests dhallDir
              , DzenDhall.Test.Parser.getTests
              , DzenDhall.Test.AST.getTests
              , DzenDhall.Test.Arguments.getTests
              , DzenDhall.Test.Animation.Marquee.getTests
              , DzenDhall.Test.Plug.getTests
              ]

  Test.Tasty.defaultMain allTests
