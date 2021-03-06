module DzenDhall.Test.Parser where

import qualified Data.HashMap.Strict as H
import           Data.Vector
import           Test.Tasty (TestTree, TestName, testGroup)
import           Test.Tasty.HUnit
import           Text.Parsec

import           DzenDhall.Config
import           DzenDhall.Data
import           DzenDhall.Parser

mkTest :: TestName -> [Token] -> Either ParseError (Bar Marshalled) -> TestTree
mkTest name tokenList expected =
  Test.Tasty.HUnit.testCase name $
    DzenDhall.Parser.runBarParser tokenList @?= expected

marquee :: Marquee
marquee = Marquee 0 0 True

getTests :: TestTree
getTests =
  testGroup "Bar data parsing"

  [ mkTest
    "parsing #1"
    [ TokOpen (OMarquee marquee)
    , TokMarkup "txt"
    , TokClose
    ]
    (Right $ Bars [ BarMarquee marquee $ Bars [ BarMarkup "txt" ] ])

  , mkTest
    "parsing #2"
    [ TokOpen (OFG $ Color "red")
    , TokMarkup "raw"
    , TokTxt "txt"
    , TokOpen (OMarquee marquee)
    , TokSource (Source { updateInterval = Nothing
                        , command = []
                        , input = ""
                        , escape = True
                        })
    , TokClose
    , TokClose
    ]

    $ Right $
    Bars [ BarProp (FG $ Color "red") $
           Bars [ BarMarkup "raw"
                , BarText "txt"
                , BarMarquee marquee $ Bars
                  [ BarSource (Source { updateInterval = Nothing
                                      , command = []
                                      , input = ""
                                      , escape = True
                                      })
                  ]
                ]
         ]

  , let fadeUp = Fade VUp 1 10
        slider = (Slider fadeUp fadeUp 1)
    in
      mkTest
      "parsing #3 - slider without separators"
      [ TokOpen (OSlider slider)
      , TokOpen (OSlider slider)
      , TokTxt "text"
      , TokClose
      , TokClose
      ]
      $ Right $
      Bars
      [ BarSlider slider $ Data.Vector.fromList
        [ Bars
          [ BarSlider slider $ Data.Vector.fromList
            [ Bars [ BarText "text" ]
            ]
          ]
        ]
      ]

  , let fadeUp = Fade VUp 1 10
        slider = (Slider fadeUp fadeUp 1)
    in
      mkTest
      "parsing #4 - slider with separators"
      [ TokOpen (OSlider slider)
      , TokOpen (OSlider slider)
      , TokTxt "a"
      , TokSeparator
      , TokTxt "b"
      , TokClose
      , TokSeparator
      , TokTxt "c"
      , TokClose
      ]
      $ Right $
      Bars
      [ BarSlider slider $ Data.Vector.fromList
        [ Bars
          [ BarSlider slider $ Data.Vector.fromList
            [ Bars
              [ BarText "a"
              ]
            , Bars
              [ BarText "b"
              ]
            ]
          ]
        , Bars
          [ BarText "c"
          ]
        ]
      ]

  , let stt = STT mempty in
      mkTest "parsing #5 - automaton"
      [ TokOpen (OAutomaton "id" stt)
      , TokOpen (OStateMapKey "a")
      , TokTxt "A"
      , TokTxt "A"
      , TokClose
      , TokOpen (OStateMapKey "b")
      , TokTxt "B"
      , TokTxt "B"
      , TokClose
      , TokOpen (OStateMapKey "c")
      , TokTxt "C"
      , TokTxt "C"
      , TokClose
      , TokClose
      ]
      $ Right $ Bars
      [ BarAutomaton "id" stt $
        H.fromList
        [ ( "a"
          , Bars [ BarText "A"
                 , BarText "A"
                 ]
          )
        , ( "b"
          , Bars [ BarText "B"
                 , BarText "B"
                 ]
          )
        , ( "c"
          , Bars [ BarText "C"
                 , BarText "C"
                 ]
          )
        ]
      ]
  ]
