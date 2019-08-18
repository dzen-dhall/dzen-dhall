module DzenDhall.Test.Plug where

import Test.Tasty (TestTree, TestName, testGroup)
import Test.Tasty.HUnit
import Text.Parsec
import DzenDhall.Plug
import Network.URI
import Data.Maybe (fromJust)

mkTest :: TestName -> String -> Either ParseError Plugin -> TestTree
mkTest name tokenList expected =
  Test.Tasty.HUnit.testCase name $
    DzenDhall.Plug.runPluginParser tokenList @?= expected

getTests :: TestTree
getTests =
  testGroup "Plug parsing"

  [ mkTest "parse github username"
    "fo0O/Bar"
    (Right (FromGithub "fo0O" "Bar"))
  , let url = "https://hackage.haskell.org/" in
      mkTest "url" url $ Right (FromURL $ fromJust $ parseURI url)
  , mkTest "FromOrg" "foo" $ Right (FromOrg "foo")
  ]
