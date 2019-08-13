{-# OPTIONS -Wno-name-shadowing #-}
module DzenDhall.Test.AST
  ( getTests
  )
where

import DzenDhall.Data
import DzenDhall.Config

import Data.Text hiding (split)
import Test.Hspec
import Test.Tasty.Hspec
import Test.Tasty

txt :: Text -> AST
txt = ASTText

getTests :: IO TestTree
getTests = testSpec "AST" $ do
  describe "DzenDhall.Data.split" $ do
    it "splits ASTText correctly" $ do
      let tree = txt (pack "123456")
      split 0 tree `shouldBe` (mempty, tree)
      split 1 tree `shouldBe` (txt "1",      txt "23456")
      split 2 tree `shouldBe` (txt "12",     txt "3456")
      split 3 tree `shouldBe` (txt "123",    txt "456")
      split 7 tree `shouldBe` (txt "123456", mempty)
      split 8 tree `shouldBe` (txt "123456", mempty)
    it "splits branches correctly" $ do
      let tree = txt "abc" <> txt "def"
      split 0 tree `shouldBe` (mempty, tree)
      split 1 tree `shouldBe` (txt "a",      txt "bc" <> txt "def")
      split 2 tree `shouldBe` (txt "ab",     txt "c"  <> txt "def")
      split 3 tree `shouldBe` (txt "abc",    txt "def")

    it "splits `ASTPadding` correctly" $ do
      let tree = ASTPadding 4 PRight (txt "a")
      split 0 tree `shouldBe` (mempty, tree)
      split 1 tree `shouldBe` (txt "a", txt "   ")
      split 2 tree `shouldBe` (txt "a" <> txt " ", txt "  ")
      split 3 tree `shouldBe` (txt "a" <> txt "  ", txt " ")
      split 4 tree `shouldBe` (txt "a" <> txt "   ", mempty)
      split 5 tree `shouldBe` (txt "a" <> txt "   ", mempty)

  describe "DzenDhall.Data.splitAST" $ do
    it "Calculates consumed lengths correctly for Txt" $ do
      let tree = txt "123456"
      splitAST 0 tree `shouldBe` EmptyL tree
      splitAST 1 tree `shouldBe` Twain (txt "1") (txt "23456") 1
      splitAST 2 tree `shouldBe` Twain (txt "12") (txt "3456") 2
      splitAST 6 tree `shouldBe` EmptyR (txt "123456") 6
      splitAST 7 tree `shouldBe` EmptyR (txt "123456") 6

    it "Calculates consumed lengths correctly for ASTs" $ do
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
