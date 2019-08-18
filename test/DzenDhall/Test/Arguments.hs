module DzenDhall.Test.Arguments where

import DzenDhall.Arguments
import Options.Applicative
import Test.Tasty
import Test.Tasty.HUnit
import Test.Hspec
import Control.Category

getTests :: TestTree
getTests =
  testGroup "Arguments"
  [ testCase "#1" $ do
      runArgParser [ "--config-dir", ".", "--dzen-binary", "dzen", "init" ]
        `shouldBe`
        pure Arguments { _mbConfigDir = Just "."
                       , _mbDzenBinary = Just "dzen"
                       , _stdoutFlag = ToDzen
                       , _mbCommand = Just Init
                       }
  ]

runArgParser :: [String] -> Maybe Arguments
runArgParser =
  execParserPure defaultPrefs argumentsParser >>>
  \case
    Options.Applicative.Success r -> Just r
    _                             -> Nothing
