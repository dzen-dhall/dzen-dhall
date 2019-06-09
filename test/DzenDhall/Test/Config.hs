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
  Prelude.sequence [ testOpeningTag           dhallDir
                   , testToken                dhallDir
                   , testSource               dhallDir
                   , testMarquee              dhallDir
                   , testBarSettings          dhallDir
                   , testConfiguration        dhallDir
                   , testPluginMeta           dhallDir
                   , testDefaultConfiguration dhallDir
                   ]

testOpeningTag :: FilePath -> IO TestTree
testOpeningTag dhallDir = do
  input <- inputWithSettings (defaultInputSettings & rootDirectory .~ dhallDir)
           (list openingTagType) [litFile|test/dhall/OpeningTag.dhall|]
  pure $ Test.Tasty.HUnit.testCase "test/dhall/OpeningTag.dhall marshalling" $
    input @?=
    [ OMarquee (Marquee 2 3), OColor "red" ]

testToken :: FilePath -> IO TestTree
testToken dhallDir = do
  input <- inputWithSettings (defaultInputSettings & rootDirectory .~ dhallDir)
           (list tokenType) [litFile|test/dhall/Token.dhall|]
  pure $ Test.Tasty.HUnit.testCase "test/dhall/Token.dhall marshalling" $
    input @?=

    [ TokOpen (OMarquee (Marquee 2 3))
    , TokRaw "raw"
    , TokSource (Source { updateInterval = Just 1000
                                , command = [ "bash" ]
                                , stdin = Just "echo 1"
                                , escapeMode = EscapeMode True True
                                })
    , TokTxt "txt"
    , TokClose ]

testSource :: FilePath -> IO TestTree
testSource dhallDir = do
  input <- inputWithSettings (defaultInputSettings & rootDirectory .~ dhallDir)
           sourceSettingsType [litFile|test/dhall/Source.dhall|]
  pure $ Test.Tasty.HUnit.testCase "test/dhall/Source.dhall marshalling" $
    input @?=

    Source { updateInterval = Just 1000
                   , command = [ "bash" ]
                   , stdin = Just "echo hi"
                   , escapeMode = EscapeMode True True
                   }

testMarquee :: FilePath -> IO TestTree
testMarquee dhallDir = do
  input <- inputWithSettings (defaultInputSettings & rootDirectory .~ dhallDir)
           marqueeType [litFile|test/dhall/Marquee.dhall|]
  pure $ Test.Tasty.HUnit.testCase "test/dhall/Marquee.dhall marshalling" $
    input @?=

    Marquee { _mqFramesPerChar = 2
                    , _mqWidth = 3
                    }

testBarSettings :: FilePath -> IO TestTree
testBarSettings dhallDir = do
  input <- inputWithSettings (defaultInputSettings & rootDirectory .~ dhallDir)
           barSettingsType [litFile|test/dhall/BarSettings.dhall|]
  pure $ Test.Tasty.HUnit.testCase "test/dhall/BarSettings.dhall marshalling" $
    input @?=

    BarSettings { _bsMonitor = 1
                , _bsExtraFlags = [ "-l", "10" ]
                , _bsUpdateInterval = 250000
                , _bsFont = Nothing
                , _bsFontWidth = Nothing
                }

testConfiguration :: FilePath -> IO TestTree
testConfiguration dhallDir = do
  input <- inputWithSettings (defaultInputSettings & rootDirectory .~ dhallDir)
           (list configurationType) [litFile|test/dhall/Configuration.dhall|]
  pure $ Test.Tasty.HUnit.testCase "test/dhall/Configuration.dhall marshalling" $
    input @?=

    [ Configuration { _cfgBarSpec = [ TokClose ]
                    , _cfgBarSettings = BarSettings { _bsMonitor = 1
                                                    , _bsExtraFlags = [ "-l", "10" ]
                                                    , _bsUpdateInterval = 250000
                                                    , _bsFont = Nothing
                                                    , _bsFontWidth = Nothing
                                                    }
                    }
    ]

testPluginMeta :: FilePath -> IO TestTree
testPluginMeta dhallDir = do
  input <- detailed $ inputWithSettings (defaultInputSettings & rootDirectory .~ dhallDir)
           pluginMetaType [litFile|test/dhall/PluginMeta.dhall|]
  pure $ Test.Tasty.HUnit.testCase "test/dhall/PluginMeta.dhall marshalling" $
    input @?= PluginMeta "1" "2" (Just "3") (Just "4") (Just "5") "6" "7" "8"

testDefaultConfiguration :: FilePath -> IO TestTree
testDefaultConfiguration dhallDir = do
  input <- detailed $ inputWithSettings (defaultInputSettings & rootDirectory .~ dhallDir)
           (list configurationType) [litFile|dhall/config.dhall|]
  pure $ Test.Tasty.HUnit.testCase "dhall/config.dhall marshalling" $ pure ()
