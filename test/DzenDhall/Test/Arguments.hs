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
                       , _explain = DontExplain
                       }
  , testCase "#2" $ do
      runArgParser [ "--explain", "plug", "foo" ]
        `shouldBe`
        pure Arguments { _mbConfigDir = Nothing
                       , _mbDzenBinary = Nothing
                       , _stdoutFlag = ToDzen
                       , _mbCommand = Just (Plug "foo")
                       , _explain = Explain
                       }
  ]

runArgParser :: [String] -> Maybe Arguments
runArgParser =
  execParserPure defaultPrefs argumentsParser >>>
  \case
    Options.Applicative.Success r -> Just r
    _                             -> Nothing
