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
  , mkTest "parsing #10"
    "click:123,scope:scope-12" $
    Just $ Click "scope-12" 123
  ]
