module DzenDhall.Test.Animation.Marquee where

import           DzenDhall.AST
import           DzenDhall.Config
import qualified DzenDhall.Animation.Marquee as Marquee

import           Test.Tasty (TestTree, TestName, testGroup)
import           Test.Tasty.HUnit

mkTest :: TestName -> Marquee -> AST -> [AST] -> TestTree
mkTest name settings ast expected =
  Test.Tasty.HUnit.testCase name $
    let frames = length expected
        actual = map (Marquee.run 10 settings ast) [0..pred frames]
    in actual @?= expected

getTests :: IO TestTree
getTests = pure $
  testGroup "Marquee"
  [ let
      ast = ASTText "12345"

      settings = Marquee 1 3

      expected = [ ASTText "123"
                 , ASTText "234"
                 , ASTText "345"
                 , ASTText "123"
                 , ASTText "234"
                 ]

    in
      mkTest "text-only" settings ast expected

  ]
