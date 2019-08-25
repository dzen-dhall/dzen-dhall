module DzenDhall.Test.Config where

import           DzenDhall.Config

import           Control.Monad
import           Dhall
import           Lens.Micro
import           System.IO (FilePath)
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.HUnit
import qualified Data.HashMap.Strict as H
import qualified Data.Text.IO


dhallDir :: FilePath
dhallDir = "./dhall"

getTests :: TestTree
getTests =
  testGroup "Config data marshalling"
  [ testGroup "Data types" $
    [ testOpeningTag
    , testToken
    , testCheck
    , testFade
    , testSource
    , testMarquee
    , testButton
    , testPadding
    , testEvent
    , testBarSettings
    , testConfiguration
    , testStateTransitionTable
    , testPluginMeta
    ]

  , testGroup "Config examples" $
    [ dummy "dhall/config.dhall"
    , dummy "test/dhall/configs/automata.dhall"
    , dummy "test/dhall/configs/assertions.dhall"
    , dummy "test/dhall/configs/scopes.dhall"
    , dummy "test/dhall/configs/variables.dhall"
    , dummy "test/dhall/configs/getEvent.dhall"
    , dummy "test/dhall/configs/deduplication.dhall"
    ]
  ]

testFile :: (Eq a, Show a) =>
            Type a -> FilePath -> a -> TestTree
testFile ty file expected  =
  Test.Tasty.HUnit.testCase (file <> " marshalling") $ do
    program <- Data.Text.IO.readFile file
    actual  <- inputWithSettings (defaultInputSettings &
                                  rootDirectory .~ dhallDir &
                                  sourceName .~ file) ty program
    actual @?= expected

testOpeningTag :: TestTree
testOpeningTag =
  testFile (list openingTagType) "test/dhall/OpeningTag.dhall"
    [ OMarquee (Marquee 2 3), OFG (Color "red"), OTrim 3 DRight ]

testToken :: TestTree
testToken =
  testFile (list tokenType) "test/dhall/Token.dhall"
    [ TokOpen (OMarquee (Marquee 2 3))
    , TokMarkup "raw"
    , TokSource (Source { updateInterval = Just 1000
                        , command = [ "bash" ]
                        , input = "echo 1"
                        , escape = True
                        })
    , TokTxt "txt"
    , TokClose ]

testSource :: TestTree
testSource =
  testFile sourceSettingsType "test/dhall/Source.dhall"
    Source { updateInterval = Just 1000
           , command = [ "bash" ]
           , input = "echo hi"
           , escape = True
           }

testMarquee :: TestTree
testMarquee =
  testFile marqueeType "test/dhall/Marquee.dhall"
    Marquee { _mqFramesPerChar = 2
            , _mqWidth = 3
            }

testButton :: TestTree
testButton =
  testFile (list buttonType) "test/dhall/Button.dhall"
    [ MouseLeft
    , MouseMiddle
    , MouseRight
    , MouseScrollUp
    , MouseScrollDown
    , MouseScrollLeft
    , MouseScrollRight
    ]

testPadding :: TestTree
testPadding =
  testFile (list paddingType) "test/dhall/Padding.dhall"
    [ PLeft, PRight, PSides ]

testEvent :: TestTree
testEvent =
  testFile (list eventType) "test/dhall/Event.dhall"
    [ Event "some text"
    ]

testCheck :: TestTree
testCheck =
  testFile (list checkType) "test/dhall/Check.dhall"
  [ Check "" $ SuccessfulExit ""
  , Check "" $ BinaryInPath ""
  ]

testFade :: TestTree
testFade =
  testFile fadeType "test/dhall/Fade.dhall" (Fade VUp 3 4)

testBarSettings :: TestTree
testBarSettings =
  testFile barSettingsType "test/dhall/Settings.dhall"
    BarSettings { _bsMonitor = 1
                , _bsExtraArgs = [ "-l", "10" ]
                , _bsUpdateInterval = 250000
                , _bsFont = Nothing
                , _bsFontWidth = 10
                }

testConfiguration :: TestTree
testConfiguration =
  testFile (list configurationType) "test/dhall/Configuration.dhall"
    [ Configuration { _cfgBarTokens = [ TokClose ]
                    , _cfgBarSettings = BarSettings { _bsMonitor = 1
                                                    , _bsExtraArgs = [ "-l", "10" ]
                                                    , _bsUpdateInterval = 250000
                                                    , _bsFont = Nothing
                                                    , _bsFontWidth = 10
                                                    }
                    }
    ]

testStateTransitionTable :: TestTree
testStateTransitionTable =
  testFile stateTransitionTableType "test/dhall/StateTransitionTable.dhall" $
    STT ( H.fromList [ (("", Event "A", "" ), ("1", []))
                     , (("", Event "B", "" ), ("1", []))
                     , (("", Event "A", "1"), ("2", []))
                     , (("", Event "B", "1"), ("2", []))
                     , (("", Event "A", "2"), ("",  []))
                     , (("", Event "B", "2"), ("",  []))
                     ]
        )

testPluginMeta :: TestTree
testPluginMeta =
  testFile pluginMetaType "test/dhall/PluginMeta.dhall" $
    PluginMeta "1" "2" (Just "3") (Just "4") (Just "5") "6" "7" 8

-- These tests assert succesful input reading (i.e. that "contracts" (in dhall terminology)
-- are not violated):

dummy :: FilePath -> TestTree
dummy file =
  Test.Tasty.HUnit.testCase (file <> " marshalling") $ do
    program <- Data.Text.IO.readFile file
    void $
      inputWithSettings (defaultInputSettings & rootDirectory .~ dhallDir)
      (list configurationType) program
