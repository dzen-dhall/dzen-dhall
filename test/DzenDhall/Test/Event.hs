module DzenDhall.Test.Event where

import Test.Tasty (TestTree, TestName, testGroup)
import Test.Tasty.HUnit

import DzenDhall.Config
import DzenDhall.Event

mkTest :: TestName -> String -> Maybe RoutedEvent -> TestTree
mkTest name input expected =
  Test.Tasty.HUnit.testCase name $
    DzenDhall.Event.parseRoutedEvent input @?= expected

getTests :: IO TestTree
getTests = pure $
  testGroup "Event parser"

  [ mkTest "parsing #1"
    "event:1,slot:name@scope" $
    Just $ RoutedEvent MouseLeft "name@scope"
  , mkTest "parsing #2"
    "event:2,slot:name@another-scope" $
    Just $ RoutedEvent MouseMiddle "name@another-scope"
  , mkTest "parsing #3"
    "event:,slot:name@scope" $
    Nothing
  , mkTest "parsing #4"
    "event:1" $
    Nothing
  , mkTest "parsing #5"
    "event:1,slot:some" $
    Nothing
  , mkTest "parsing #6"
    "event:1,slot:some@" $
    Nothing
  , mkTest "parsing #7"
    "event:1,slot:some-name@scope" $
    Nothing
  ]
