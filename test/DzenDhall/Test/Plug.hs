module DzenDhall.Test.Plug where

import Test.Tasty (TestTree, TestName, testGroup)
import Test.Tasty.HUnit
import Text.Parsec
import DzenDhall.Plug
import Network.URI
import Data.Maybe (fromJust)

mkTest :: TestName -> String -> Either ParseError PluginSourceSpec -> TestTree
mkTest name input expected =
  Test.Tasty.HUnit.testCase name $
    DzenDhall.Plug.parseSourceSpec input @?= expected

getTests :: TestTree
getTests =
  testGroup "Plug parsing"

  [ mkTest "FromGithub #0"
    "fo0O/Bar"
    (Right (FromGithub "fo0O" "Bar" "master"))
  , mkTest "FromGithub #1"
    "fo0-O/Ba_r@dev-elopv1.1"
    (Right (FromGithub "fo0-O" "Ba_r" "dev-elopv1.1"))
  , let url = "https://hackage.haskell.org/" in
      mkTest "url" url $ Right (FromURL $ fromJust $ parseURI url)
  , mkTest "FromOrg" "foo" $ Right (FromOrg "foo" "master")
  , mkTest "FromOrg" "foo@develop" $ Right (FromOrg "foo" "develop")
  ]
