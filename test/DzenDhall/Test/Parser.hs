module DzenDhall.Test.Parser where

import DzenDhall.Parse
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import Text.Parsec
import DzenDhall.Config
import DzenDhall.Tree

getTests :: IO TestTree
getTests = pure $
  testGroup "Plugin data parsing"
  [ Test.Tasty.HUnit.testCase "parsing #1" $
    runParser plugin () "" [ TokOpen (OMarquee 3)
                           , TokRaw "txt"
                           , TokClose
                           ] @?=
    Right (Marquee 3 $ Plugins [ Raw "txt" ])
  ]
