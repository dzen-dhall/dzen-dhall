module DzenDhall.Test.Event where

import Test.Tasty (TestTree, TestName, testGroup)
import Test.Tasty.HUnit

import DzenDhall.Config
import DzenDhall.Event

mkTest :: TestName -> String -> Maybe PipeCommand -> TestTree
mkTest name input expected =
  Test.Tasty.HUnit.testCase name $
    DzenDhall.Event.parsePipeCommand input @?= expected

getTests :: TestTree
getTests =
  testGroup "PipeCommand parser"

  [ mkTest "parsing #3"
    "event:@scope" $
    Nothing
  , mkTest "parsing #4"
    "event:1" $
    Nothing
  , mkTest "parsing #5"
    "event:1some" $
    Nothing
  , mkTest "parsing #6"
    "event:1@" $
    Nothing
  , mkTest "parsing #9"
    "event:MyEvent@scope" $
    Just $ RoutedEvent (Event "MyEvent") "scope"
  , mkTest "parsing #10"
    "click:123@scope-12" $
    Just $ Click "scope-12" 123
  ]
