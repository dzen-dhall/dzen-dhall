module DzenDhall.Test.Event where

import Test.Tasty (TestTree, TestName, testGroup)
import Test.Tasty.HUnit

import DzenDhall.Config
import DzenDhall.Event
import DzenDhall.Runtime.Data

mkTest :: TestName -> String -> Maybe RoutedEvent -> TestTree
mkTest name input expected =
  Test.Tasty.HUnit.testCase name $
    DzenDhall.Event.parseRoutedEvent input @?= expected

getTests :: IO TestTree
getTests = pure $
  testGroup "Event parser"

  [ mkTest "parsing #1"
    "event:1,slot:SLOT@scope" $
    Just $ RoutedEvent (MouseEvent MouseLeft) "SLOT" "scope"
  , mkTest "parsing #2"
    "event:2,slot:SLOT@another-scope" $
    Just $ RoutedEvent (MouseEvent MouseMiddle) "SLOT" "another-scope"
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
  -- slot name must start with [A-Z]
  , mkTest "parsing #8"
    "event:2,slot:slot@another-scope" $
    Nothing
  , mkTest "parsing #9"
    "event:MyEvent,slot:SLOT@scope" $
    Just $ RoutedEvent (CustomEvent "MyEvent") "SLOT" "scope"
  ]
