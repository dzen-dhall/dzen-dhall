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
    "id:1,event:1" $
    Just $ RoutedEvent MouseLeft 1
  , mkTest "parsing #2"
    "id:10,event:2" $
    Just $ RoutedEvent MouseMiddle 10
  , mkTest "parsing #3"
    "id:1,event:" $
    Nothing
  , mkTest "parsing #4"
    "id:,event:1" $
    Nothing
  , mkTest "parsing #5"
    "id:,event:" $
    Nothing
  ]
