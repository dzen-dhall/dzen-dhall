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


getTests :: FilePath -> TestTree
getTests dhallDir =
  testGroup "Config data marshalling"
  [ testGroup "Data types" $
    [ testOpeningTag
    , testToken
    , testCheck
    , testSource
    , testMarquee
    , testButton
    , testPadding
    , testEvent
    , testBarSettings
    , testConfiguration
    , testStateTransitionTable
    , testPluginMeta
    ] <&> ($ dhallDir)

  , testGroup "Config examples" $
    [ dummy "dhall/config.dhall"
    , dummy "test/dhall/configs/automata.dhall"
    , dummy "test/dhall/configs/assertions.dhall"
    ] <&> ($ dhallDir)
  ]

testFile :: (Eq a, Show a) =>
            Type a -> FilePath -> a -> FilePath -> TestTree
testFile ty file expected dhallDir  =
  Test.Tasty.HUnit.testCase (file <> " marshalling") $ do
    program <- Data.Text.IO.readFile file
    actual  <- inputWithSettings (defaultInputSettings &
                                  rootDirectory .~ dhallDir &
                                  sourceName .~ file) ty program
    actual @?= expected

testOpeningTag :: FilePath -> TestTree
testOpeningTag =
  testFile (list openingTagType) "test/dhall/OpeningTag.dhall"
    [ OMarquee (Marquee 2 3), OFG (Color "red"), OTrim 3 DRight ]

testToken :: FilePath -> TestTree
testToken =
  testFile (list tokenType) "test/dhall/Token.dhall"
    [ TokOpen (OMarquee (Marquee 2 3))
    , TokMarkup "raw"
    , TokSource (Source { updateInterval = Just 1000
                        , command = [ "bash" ]
                        , input = "echo 1"
                        , escapeMode = EscapeMode True True
                        })
    , TokTxt "txt"
    , TokClose ]

testSource :: FilePath -> TestTree
testSource =
  testFile sourceSettingsType "test/dhall/Source.dhall"
    Source { updateInterval = Just 1000
           , command = [ "bash" ]
           , input = "echo hi"
           , escapeMode = EscapeMode True True
           }

testMarquee :: FilePath -> TestTree
testMarquee =
  testFile marqueeType "test/dhall/Marquee.dhall"
    Marquee { _mqFramesPerChar = 2
            , _mqWidth = 3
            }

testButton :: FilePath -> TestTree
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

testPadding :: FilePath -> TestTree
testPadding =
  testFile (list paddingType) "test/dhall/Padding.dhall"
    [ PLeft, PRight, PSides ]

testEvent :: FilePath -> TestTree
testEvent =
  testFile (list eventType) "test/dhall/Event.dhall"
    [ CustomEvent "some text"
    , MouseEvent MouseLeft
    ]

testCheck :: FilePath -> TestTree
testCheck =
  testFile (list checkType) "test/dhall/Check.dhall"
  [ Check "" $ SuccessfulExit ""
  , Check "" $ BinaryInPath ""
  ]

testBarSettings :: FilePath -> TestTree
testBarSettings =
  testFile barSettingsType "test/dhall/BarSettings.dhall"
    BarSettings { _bsMonitor = 1
                , _bsExtraArgs = [ "-l", "10" ]
                , _bsUpdateInterval = 250000
                , _bsFont = Nothing
                , _bsFontWidth = Nothing
                }

testConfiguration :: FilePath -> TestTree
testConfiguration =
  testFile (list configurationType) "test/dhall/Configuration.dhall"
    [ Configuration { _cfgBarTokens = [ TokClose ]
                    , _cfgBarSettings = BarSettings { _bsMonitor = 1
                                                    , _bsExtraArgs = [ "-l", "10" ]
                                                    , _bsUpdateInterval = 250000
                                                    , _bsFont = Nothing
                                                    , _bsFontWidth = Nothing
                                                    }
                    }
    ]

testStateTransitionTable :: FilePath -> TestTree
testStateTransitionTable =
  testFile stateTransitionTableType "test/dhall/StateTransitionTable.dhall" $
    STT ( H.fromList [ (("a", "", MouseEvent MouseLeft,  ""), ("1", []))
                     , (("a", "", MouseEvent MouseRight, ""), ("1", []))
                     , (("b", "", MouseEvent MouseLeft,  ""), ("1", []))
                     , (("b", "", MouseEvent MouseRight, ""), ("1", []))
                     , (("a", "", MouseEvent MouseLeft,  "1"), ("2", []))
                     , (("a", "", MouseEvent MouseRight, "1"), ("2", []))
                     , (("b", "", MouseEvent MouseLeft,  "1"), ("2", []))
                     , (("b", "", MouseEvent MouseRight, "1"), ("2", []))
                     , (("a", "", MouseEvent MouseLeft,  "2"), ("", []))
                     , (("a", "", MouseEvent MouseRight, "2"), ("", []))
                     , (("b", "", MouseEvent MouseLeft,  "2"), ("", []))
                     , (("b", "", MouseEvent MouseRight, "2"), ("", []))
                     ]
        )

testPluginMeta :: FilePath -> TestTree
testPluginMeta =
  testFile pluginMetaType "test/dhall/PluginMeta.dhall" $
    PluginMeta "1" "2" (Just "3") (Just "4") (Just "5") "6" "7" 8

-- These tests assert succesful input reading (i.e. that "contracts" (in dhall terminology)
-- are not violated):

dummy :: FilePath -> FilePath -> TestTree
dummy file dhallDir =
  Test.Tasty.HUnit.testCase (file <> " marshalling") $ do
    program <- Data.Text.IO.readFile file
    void $ detailed $
      inputWithSettings (defaultInputSettings & rootDirectory .~ dhallDir)
      (list configurationType) program
