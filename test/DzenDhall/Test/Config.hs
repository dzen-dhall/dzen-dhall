{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS -Wno-name-shadowing #-}
module DzenDhall.Test.Config where

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
  Prelude.sequence [ testOpeningTag      dhallDir
                   , testToken           dhallDir
                   , testSourceSettings  dhallDir
                   , testMarqueeSettings dhallDir
                   , testBarSettings     dhallDir
                   , testConfiguration   dhallDir
                   ]

testOpeningTag :: FilePath -> IO TestTree
testOpeningTag dhallDir = do
  input <- inputWithSettings (defaultInputSettings & rootDirectory .~ dhallDir)
           (list openingTagType) [litFile|test/dhall/OpeningTag.dhall|]
  pure $ Test.Tasty.HUnit.testCase "OpeningTag marshalling" $
    [ OMarquee 3, OColor "red" ] @?= input

testToken :: FilePath -> IO TestTree
testToken dhallDir = do
  input <- inputWithSettings (defaultInputSettings & rootDirectory .~ dhallDir)
           (list tokenType) [litFile|test/dhall/Token.dhall|]
  pure $ Test.Tasty.HUnit.testCase "Token marshalling" $
    [ TokOpen (OMarquee 1)
    , TokRaw "raw"
    , TokSource (SourceSettings { updateInterval = Just 1
                                , command = [ "bash" ]
                                , stdin = Just "echo 1"
                                , escapeMode = EscapeMode True True
                                })
    , TokTxt "txt"
    , TokClose ] @?= input

testSourceSettings :: FilePath -> IO TestTree
testSourceSettings dhallDir = do
  input <- inputWithSettings (defaultInputSettings & rootDirectory .~ dhallDir)
           sourceSettingsType [litFile|test/dhall/SourceSettings.dhall|]
  pure $ Test.Tasty.HUnit.testCase "SourceSettings marshalling" $
    SourceSettings { updateInterval = Just 1
                   , command = [ "bash" ]
                   , stdin = Just "echo hi"
                   , escapeMode = EscapeMode True True
                   }
    @?= input

testMarqueeSettings :: FilePath -> IO TestTree
testMarqueeSettings dhallDir = do
  input <- inputWithSettings (defaultInputSettings & rootDirectory .~ dhallDir)
           marqueeSettingsType [litFile|test/dhall/MarqueeSettings.dhall|]
  pure $ Test.Tasty.HUnit.testCase "MarqueeSettings marshalling" $
    MarqueeSettings { mqSpeed = 1
                    , mqFramesPerChar = 2
                    , mqWidth = 3
                    }
    @?= input

testBarSettings :: FilePath -> IO TestTree
testBarSettings dhallDir = do
  input <- inputWithSettings (defaultInputSettings & rootDirectory .~ dhallDir)
           barSettingsType [litFile|test/dhall/BarSettings.dhall|]
  pure $ Test.Tasty.HUnit.testCase "MarqueeSettings marshalling" $
    BarSettings { bsMonitor = 1
                , bsExtraFlags = [ "-l", "10" ]
                , bsUpdateInterval = 250000
                }
    @?= input

testConfiguration :: FilePath -> IO TestTree
testConfiguration dhallDir = do
  input <- inputWithSettings (defaultInputSettings & rootDirectory .~ dhallDir)
           (list configurationType) [litFile|test/dhall/Configuration.dhall|]
  pure $ Test.Tasty.HUnit.testCase "MarqueeSettings marshalling" $
    [ Configuration { bar = [ TokClose ]
                    , settings = BarSettings { bsMonitor = 1
                                             , bsExtraFlags = [ "-l", "10" ]
                                             , bsUpdateInterval = 250000
                                             }
                    }
    ]
    @?= input
