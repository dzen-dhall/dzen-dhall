module DzenDhall.Test.Arguments where

import DzenDhall.Arguments
import Options.Applicative
import Test.Tasty (TestTree, testGroup)
import Test.Hspec
import Test.Tasty.Hspec
import Control.Category

getTests :: IO TestTree
getTests = testSpec "Argument parser" $ do
  describe "parses arguments correctly" $ do
    it "test #1" $ do
      runArgParser [ "--config-dir", ".", "--dzen-binary", "dzen", "init" ]
        `shouldBe`
        pure Arguments { configPath = Just "."
                       , dzenBinary = Just "dzen"
                       , mbCommand = Just Init
                       }

runArgParser :: [String] -> Maybe Arguments
runArgParser =
  execParserPure defaultPrefs parserInfo >>>
  \case
    Options.Applicative.Success r -> Just r
    _                             -> Nothing
