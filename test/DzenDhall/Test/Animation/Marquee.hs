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

getTests :: TestTree
getTests =
  testGroup "Marquee"
  [ let
      ast = ASTText "12345"

      settings = Marquee 1 3 False

      expected = [ ASTText "123"
                 , ASTText "234"
                 , ASTText "345"
                 , ASTs (ASTText "45") (ASTText "1")
                 , ASTs (ASTText "5") (ASTText "12")

                 , ASTText "123"
                 , ASTText "234"
                 , ASTText "345"
                 , ASTs (ASTText "45") (ASTText "1")
                 ]
    in
      mkTest "shouldNotWrap #0" settings ast expected

  , let
      ast = ASTText "123"

      settings = Marquee 1 5 False

      expected = [ ASTs (ASTText "123") (ASTText "  ")
                 , ASTs (ASTText "123") (ASTText "  ")
                 ]

    in
      mkTest "shouldNotWrap #1" settings ast expected

  , let
      ast = ASTText "12345"

      settings = Marquee 1 3 True

      expected = [ ASTText "123"
                 , ASTText "234"
                 , ASTText "345"
                 , ASTs (ASTText "45") (ASTText "1")
                 , ASTs (ASTText "5") (ASTText "12")
                 , (ASTText "123")
                 ]

    in
      mkTest "shouldWrap #0" settings ast expected

  ]
