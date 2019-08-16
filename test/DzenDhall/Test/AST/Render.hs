module DzenDhall.Test.AST.Render where

import DzenDhall.AST
import DzenDhall.AST.Render
import DzenDhall.Data
import DzenDhall.Config

import Test.Hspec
import Test.Tasty.Hspec
import Test.Tasty

asts :: [AST] -> AST
asts = foldr (<>) EmptyAST

getTests :: IO TestTree
getTests = testSpec "DzenDhall.AST.Render" $ do
  describe "DzenDhall.AST.Render.runRender" $ do
    it "renders nested colors correctly #0" $ do
      let tree =
            asts [ ASTProp (FG (Color "black")) $
                   ASTText "text"
                 , ASTText "..."
                 ]
      runRender tree `shouldBe` "^fg(black)text^fg()..."

    it "renders nested colors correctly #1" $ do
      let tree =
            ASTProp (FG (Color "black")) $
            ASTProp (FG (Color "white")) $
            ASTText "text"
      runRender tree `shouldBe` "^fg(black)^fg(white)text^fg(black)^fg()"

    it "renders nested colors correctly #2" $ do
      let tree =
            ASTProp (FG (Color "black")) $
            asts [ ASTProp (FG (Color "white")) $
                   ASTText "text1"
                 , ASTText "text1.1"
                 , ASTProp (FG (Color "red")) $
                   ASTText "text2"
                 , ASTText "text3"
                 ]
      runRender tree `shouldBe`
        "^fg(black)^fg(white)text1^fg(black)text1.1^fg(red)text2^fg(black)text3^fg()"

    it "renders nested colors correctly #3" $ do
      let tree =
            ASTProp (FG (Color "red"))
            (ASTs (ASTs (ASTText "a")
                    (ASTProp (FG (Color "green"))
                     (ASTText "b")))
             (ASTText "c"))
      runRender tree `shouldBe` "^fg(red)a^fg(green)b^fg(red)c^fg()"
