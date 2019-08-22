{-# LANGUAGE QuasiQuotes #-}
module Main where

import qualified DzenDhall.Test.AST
import qualified DzenDhall.Test.AST.Render
import qualified DzenDhall.Test.Animation.Marquee
import qualified DzenDhall.Test.Arguments
import qualified DzenDhall.Test.Config
import qualified DzenDhall.Test.Event
import qualified DzenDhall.Test.Parser
import qualified DzenDhall.Test.Plug

import qualified GHC.IO.Encoding
import qualified System.IO
import           Test.Tasty

main :: IO ()
main = do
  GHC.IO.Encoding.setLocaleEncoding System.IO.utf8

  Test.Tasty.defaultMain $
    testGroup "DzenDhall"
    [ DzenDhall.Test.Config.getTests
    , DzenDhall.Test.Parser.getTests
    , DzenDhall.Test.AST.getTests
    , DzenDhall.Test.AST.Render.getTests
    , DzenDhall.Test.Arguments.getTests
    , DzenDhall.Test.Animation.Marquee.getTests
    , DzenDhall.Test.Plug.getTests
    , DzenDhall.Test.Event.getTests
    ]
