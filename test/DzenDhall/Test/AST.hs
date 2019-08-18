{-# OPTIONS -Wno-name-shadowing -Wno-orphans #-}
{-# LANGUAGE TemplateHaskell #-}
module DzenDhall.Test.AST
  ( getTests
  )
where

import DzenDhall.AST
import DzenDhall.Config
import DzenDhall.Data

import           Control.Arrow
import           Data.Text hiding (split)
import           Test.Hspec
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.QuickCheck (CoArbitrary (..), Arbitrary (..), counterexample, withMaxSuccess, (===), (==>))
import qualified Test.QuickCheck as QC
import           Test.QuickCheck.Arbitrary ()
import           Generic.Random
import qualified Test.Tasty.QuickCheck as QC

txt :: Text -> AST
txt = ASTText

getTests :: TestTree
getTests =
  testGroup "AST"
  [ testGroup "DzenDhall.Data.split" $
    [ testCase "splits ASTText correctly" $ do
        let tree = txt (pack "123456")
        split 0 tree `shouldBe` (mempty, tree)
        split 1 tree `shouldBe` (txt "1",      txt "23456")
        split 2 tree `shouldBe` (txt "12",     txt "3456")
        split 3 tree `shouldBe` (txt "123",    txt "456")
        split 7 tree `shouldBe` (txt "123456", mempty)
        split 8 tree `shouldBe` (txt "123456", mempty)

    , testCase "splits branches correctly" $ do
        let tree = txt "abc" <> txt "def"
        split 0 tree `shouldBe` (mempty, tree)
        split 1 tree `shouldBe` (txt "a",      txt "bc" <> txt "def")
        split 2 tree `shouldBe` (txt "ab",     txt "c"  <> txt "def")
        split 3 tree `shouldBe` (txt "abc",    txt "def")

    , testCase "splits `ASTPadding` correctly" $ do
        let tree = ASTPadding 4 PRight (txt "a")
        split 0 tree `shouldBe` (mempty, tree)
        split 1 tree `shouldBe` (txt "a", txt "   ")
        split 2 tree `shouldBe` (txt "a" <> txt " ", txt "  ")
        split 3 tree `shouldBe` (txt "a" <> txt "  ", txt " ")
        split 4 tree `shouldBe` (txt "a" <> txt "   ", mempty)
        split 5 tree `shouldBe` (txt "a" <> txt "   ", mempty)

    , testCase "DzenDhall.Data.splitAST calculates consumed lengths correctly for Txt" $ do
        let tree = txt "123456"
        splitAST 0 tree `shouldBe` EmptyL tree
        splitAST 1 tree `shouldBe` Twain (txt "1") (txt "23456") 1
        splitAST 2 tree `shouldBe` Twain (txt "12") (txt "3456") 2
        splitAST 6 tree `shouldBe` EmptyR (txt "123456") 6
        splitAST 7 tree `shouldBe` EmptyR (txt "123456") 6

    , testCase "Calculates consumed lengths correctly for ASTs" $ do
        let tree = txt "123" <> txt "456"
        splitAST 0 tree `shouldBe` EmptyL tree
        splitAST 1 tree `shouldBe` Twain (txt "1") (txt "23" <> txt "456") 1
        splitAST 2 tree `shouldBe` Twain (txt "12") (txt "3" <> txt "456") 2
        splitAST 6 tree `shouldBe` EmptyR tree 6
        splitAST 7 tree `shouldBe` EmptyR tree 6

        let tree = txt "12" <> txt "34" <> txt "56"
        splitAST 0 tree `shouldBe` EmptyL tree
        splitAST 1 tree `shouldBe` Twain (txt "1")
                                         (txt "2" <> txt "34" <> txt "56")
                                         1
        splitAST 2 tree `shouldBe` Twain (txt "12")
                                         (txt "34" <> txt "56")
                                         2
        splitAST 3 tree `shouldBe` Twain (txt "12" <> txt "3")
                                         (txt "4" <> txt "56")
                                         3
        splitAST 4 tree `shouldBe` Twain (txt "12" <> txt "34")
                                         (txt "56")
                                         4
        splitAST 5 tree `shouldBe` Twain (txt "12" <> txt "34" <> txt "5")
                                         (txt "6")
                                         5
        splitAST 6 tree `shouldBe` EmptyR (txt "12" <> txt "34" <> txt "56") 6
        splitAST 7 tree `shouldBe` EmptyR (txt "12" <> txt "34" <> txt "56") 6

        let tree = (txt "12" <> txt "34") <> txt "56"
        splitAST 0 tree `shouldBe` EmptyL tree
        splitAST 1 tree `shouldBe` Twain (txt "1")
                                         ((txt "2" <> txt "34") <> txt "56")
                                         1
        splitAST 2 tree `shouldBe` Twain (txt "12")
                                         (txt "34" <> txt "56")
                                         2
        splitAST 3 tree `shouldBe` Twain (txt "12" <> txt "3")
                                         (txt "4" <> txt "56")
                                         3
        splitAST 4 tree `shouldBe` Twain (txt "12" <> txt "34")
                                         (txt "56")
                                         4
        splitAST 5 tree `shouldBe` Twain ((txt "12" <> txt "34") <> txt "5")
                                         (txt "6")
                                         5
        splitAST 6 tree `shouldBe` EmptyR ((txt "12" <> txt "34") <> txt "56") 6
        splitAST 7 tree `shouldBe` EmptyR ((txt "12" <> txt "34") <> txt "56") 6
    ]
  , testGroup "Generic tests"
    [ QC.testProperty "AST.split preserves lengths"
      prop_ast_split_preserves_widths
    , QC.testProperty "AST.split returns ASTs with correct lengths"
      prop_ast_split_returns_correct_lengths
    ]
  ]

prop_ast_split_preserves_widths :: AST -> Int -> QC.Property
prop_ast_split_preserves_widths ast position =
  withMaxSuccess 10000 $
  counterexample (show $ split position ast) $
  astWidth ast === (uncurry (+) $ astWidth *** astWidth $ split position ast)

prop_ast_split_returns_correct_lengths :: AST -> Int -> QC.Property
prop_ast_split_returns_correct_lengths ast position =
  withMaxSuccess 10000 $
  position >= 0 && astWidth ast >= position ==>
  counterexample (show $ split position ast) $
  astWidth (fst (split position ast)) === position

instance Arbitrary Text where
  arbitrary = Data.Text.pack <$> arbitrary

instance CoArbitrary Text where
  coarbitrary = coarbitrary . Data.Text.unpack

instance Arbitrary Color where
  arbitrary = genericArbitraryU

instance CoArbitrary Color

instance Arbitrary Button where
  arbitrary = genericArbitraryU

instance CoArbitrary Button

instance Arbitrary ClickableArea where
  arbitrary = genericArbitraryU

instance CoArbitrary ClickableArea

instance Arbitrary Position where
  arbitrary = genericArbitraryU

instance CoArbitrary Position

instance Arbitrary AbsolutePosition where
  arbitrary = genericArbitraryU

instance CoArbitrary AbsolutePosition

instance Arbitrary Property where
  arbitrary = genericArbitraryU

instance CoArbitrary Property

instance Arbitrary Shape where
  arbitrary = genericArbitraryU

instance CoArbitrary Shape

instance Arbitrary Padding where
  arbitrary = genericArbitraryU

instance CoArbitrary Padding

instance Arbitrary AST where
  arbitrary = genericArbitraryU

instance CoArbitrary AST
