module DzenDhall.Test.Parser where

import DzenDhall.Config
import DzenDhall.Data
import DzenDhall.Parser
import Test.Tasty (TestTree, TestName, testGroup)
import Test.Tasty.HUnit
import Text.Parsec

mkTest :: TestName -> [Token] -> Either ParseError (Bar Source) -> TestTree
mkTest name tokenList expected =
  Test.Tasty.HUnit.testCase name $
    DzenDhall.Parser.runBarParser tokenList @?= expected

marquee = Marquee 0 0

getTests :: IO TestTree
getTests = pure $
  testGroup "Bar data parsing"

  [ mkTest
    "parsing #1"
    [ TokOpen (OMarquee marquee)
    , TokRaw "txt"
    , TokClose
    ]
    (Right $ Bars [ BarMarquee marquee $ Bars [ BarRaw "txt" ] ])

  , mkTest
    "parsing #2"
    [ TokOpen (OColor "red")
    , TokRaw "raw"
    , TokTxt "txt"
    , TokOpen (OMarquee marquee)
    , TokSource (Source { updateInterval = Nothing
                                , command = []
                                , stdin = Nothing
                                , escapeMode = EscapeMode True True
                                })
    , TokClose
    , TokClose
    ]

    $ Right $
    Bars [ BarColor "red" $
           Bars [ BarRaw "raw"
                , BarText "txt"
                , BarMarquee marquee $ Bars
                  [ BarSource (Source { updateInterval = Nothing
                                              , command = []
                                              , stdin = Nothing
                                              , escapeMode = EscapeMode True True
                                              })
                  ]
                ]
         ]
  ]
