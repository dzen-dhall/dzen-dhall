{-# LANGUAGE QuasiQuotes #-}
module DzenDhall.Test.Config where

import Data.Functor
import Dhall
import DzenDhall.Config
import FileQuoter
import Lens.Micro
import System.IO (FilePath)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

getTests :: FilePath -> IO TestTree
getTests dhallDir =
  testGroup "Config data marshalling" <$>
  Prelude.sequence [ testOpeningTag     dhallDir
                   , testToken          dhallDir
                   , testSourceSettings dhallDir
                   ]

testOpeningTag :: FilePath -> IO TestTree
testOpeningTag dhallDir = do
  input <- inputWithSettings (defaultInputSettings & rootDirectory .~ dhallDir)
          (list openingTag) [litFile|test/dhall/OpeningTag.dhall|]
  pure $ Test.Tasty.HUnit.testCase "OpeningTag marshalling" $
    [ OMarquee 3, OColor "red" ] @?= input

testToken :: FilePath -> IO TestTree
testToken dhallDir = do
  input <- inputWithSettings (defaultInputSettings & rootDirectory .~ dhallDir)
          (list token) [litFile|test/dhall/Token.dhall|]
  pure $ Test.Tasty.HUnit.testCase "Token marshalling" $
    [ TokOpen (OMarquee 1)
    , TokRaw "raw"
    , TokSource "shell"
    , TokTxt "txt"
    , TokClose ] @?= input

testSourceSettings :: FilePath -> IO TestTree
testSourceSettings dhallDir = do
  input <- inputWithSettings (defaultInputSettings & rootDirectory .~ dhallDir)
          sourceSettings [litFile|test/dhall/SourceSettings.dhall|]
  pure $ Test.Tasty.HUnit.testCase "SourceSettings marshalling" $
    SourceSettings { updateInterval = Just 1
                   , command = [ "bash" ]
                   , stdin = Just "echo hi"
                   }
    @?= input
