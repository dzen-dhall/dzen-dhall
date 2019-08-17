module DzenDhall.Test.Config where

import qualified Data.HashMap.Strict as H
import           Dhall
import           Lens.Micro
import qualified Data.Text.IO
import           System.IO (FilePath)
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.HUnit

import           DzenDhall.Config


getTests :: FilePath -> IO TestTree
getTests dhallDir =
  testGroup "Config data marshalling" <$>
  Prelude.sequence [ testOpeningTag           dhallDir
                   , testToken                dhallDir
                   , testSource               dhallDir
                   , testMarquee              dhallDir
                   , testButton               dhallDir
                   , testPadding              dhallDir
                   , testEvent                dhallDir
                   , testBarSettings          dhallDir
                   , testConfiguration        dhallDir
                   , testStateTransitionTable dhallDir
                   , testPluginMeta           dhallDir
                   , testDefaultConfiguration dhallDir
                   ]

testFile :: (Eq a, Show a) =>
            Type a -> FilePath -> a -> FilePath -> IO TestTree
testFile ty file expected dhallDir  = do
  program <- Data.Text.IO.readFile file
  actual  <- inputWithSettings (defaultInputSettings &
                                rootDirectory .~ dhallDir &
                                sourceName .~ file) ty program
  pure $ Test.Tasty.HUnit.testCase (file <> " marshalling") $
    actual @?= expected

testOpeningTag :: FilePath -> IO TestTree
testOpeningTag =
  testFile (list openingTagType) "test/dhall/OpeningTag.dhall"
    [ OMarquee (Marquee 2 3), OFG (Color "red"), OTrim 3 DRight ]

testToken :: FilePath -> IO TestTree
testToken =
  testFile (list tokenType) "test/dhall/Token.dhall"
    [ TokOpen (OMarquee (Marquee 2 3))
    , TokRaw "raw"
    , TokSource (Source { updateInterval = Just 1000
                        , command = [ "bash" ]
                        , input = "echo 1"
                        , escapeMode = EscapeMode True True
                        })
    , TokTxt "txt"
    , TokClose ]

testSource :: FilePath -> IO TestTree
testSource =
  testFile sourceSettingsType "test/dhall/Source.dhall"
    Source { updateInterval = Just 1000
           , command = [ "bash" ]
           , input = "echo hi"
           , escapeMode = EscapeMode True True
           }

testMarquee :: FilePath -> IO TestTree
testMarquee =
  testFile marqueeType "test/dhall/Marquee.dhall"
    Marquee { _mqFramesPerChar = 2
            , _mqWidth = 3
            }

testButton :: FilePath -> IO TestTree
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

testPadding :: FilePath -> IO TestTree
testPadding =
  testFile (list paddingType) "test/dhall/Padding.dhall"
    [ PLeft, PRight, PSides ]

testEvent :: FilePath -> IO TestTree
testEvent =
  testFile (list eventType) "test/dhall/Event.dhall"
    [ CustomEvent "some text"
    , MouseEvent MouseLeft
    ]

testBarSettings :: FilePath -> IO TestTree
testBarSettings =
  testFile barSettingsType "test/dhall/BarSettings.dhall"
    BarSettings { _bsMonitor = 1
                , _bsExtraFlags = [ "-l", "10" ]
                , _bsUpdateInterval = 250000
                , _bsFont = Nothing
                , _bsFontWidth = Nothing
                }

testConfiguration :: FilePath -> IO TestTree
testConfiguration =
  testFile (list configurationType) "test/dhall/Configuration.dhall"
    [ Configuration { _cfgBarTokens = [ TokClose ]
                    , _cfgBarSettings = BarSettings { _bsMonitor = 1
                                                    , _bsExtraFlags = [ "-l", "10" ]
                                                    , _bsUpdateInterval = 250000
                                                    , _bsFont = Nothing
                                                    , _bsFontWidth = Nothing
                                                    }
                    }
    ]

testStateTransitionTable :: FilePath -> IO TestTree
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

testPluginMeta :: FilePath -> IO TestTree
testPluginMeta =
  testFile pluginMetaType "test/dhall/PluginMeta.dhall" $
    PluginMeta "1" "2" (Just "3") (Just "4") (Just "5") "6" "7" 8

testDefaultConfiguration :: FilePath -> IO TestTree
testDefaultConfiguration dhallDir = do
  let file = "dhall/config.dhall"
  program <- Data.Text.IO.readFile file
  -- This test just asserts succesful input reading
  _input <- detailed $ inputWithSettings (defaultInputSettings & rootDirectory .~ dhallDir)
           (list configurationType) program
  pure $ Test.Tasty.HUnit.testCase (file <> " marshalling") $ pure ()
