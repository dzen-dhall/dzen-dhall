{-# LANGUAGE QuasiQuotes #-}
module DzenDhall.Test.Config where

import DzenDhall.Config
import Test.Tasty (TestTree, testGroup)
import System.IO (FilePath)
import Test.Tasty.HUnit
import Lens.Micro
import FileQuoter
import Dhall
import Data.Functor

getTests :: FilePath -> IO TestTree
getTests dhallDir =
  testGroup "Config data marshalling" <$>
  Prelude.sequence [ testOpeningTag dhallDir
                   , testToken      dhallDir
                   ]

testOpeningTag :: FilePath -> IO TestTree
testOpeningTag dhallDir = do
  input <- inputWithSettings (defaultInputSettings & rootDirectory .~ dhallDir)
          (list openingTag) [litFile|test/dhall/OpeningTag.dhall|]
  pure $ Test.Tasty.HUnit.testCase "OpeningTag marshalling" $
    [OMarquee 3, OColor "red"] @?= input

testToken :: FilePath -> IO TestTree
testToken dhallDir = do
  input <- inputWithSettings (defaultInputSettings & rootDirectory .~ dhallDir)
          (list token) [litFile|test/dhall/Token.dhall|]
  pure $ Test.Tasty.HUnit.testCase "Token marshalling" $
    [ TokOpen (OMarquee 1)
    , TokRaw "raw"
    , TokShell "shell"
    , TokTxt "txt"
    , TokClose ] @?= input
