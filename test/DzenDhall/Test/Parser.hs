module DzenDhall.Test.Parser where

import DzenDhall.Config
import DzenDhall.Data
import DzenDhall.Parser
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import Text.Parsec

mkTest name tokens expected =
  Test.Tasty.HUnit.testCase name $
    runParser bar () name tokens @?= expected

getTests :: IO TestTree
getTests = pure $
  testGroup "Bar data parsing"

  [ mkTest
    "parsing #1"
    [ TokOpen (OMarquee 3)
    , TokRaw "txt"
    , TokClose
    ]
    (Right $ Marquee 3 $ Bars [ Raw "txt" ])

  , mkTest
    "parsing #2"
    [ TokOpen (OColor "red")
    , TokRaw "raw"
    , TokTxt "txt"
    , TokOpen (OMarquee 0)
    , TokSource (SourceSettings { updateInterval = Nothing
                                , command = []
                                , stdin = Nothing
                                })
    , TokClose
    , TokClose
    ]
    $ Right $ Color "red" $ Bars
    [ Raw "raw"
    , Txt "txt"
    , Marquee 0 $ Bars
      [ Source (SourceSettings { updateInterval = Nothing
                               , command = []
                               , stdin = Nothing
                               })
      ]
    ]
  ]
