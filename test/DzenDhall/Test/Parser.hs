module DzenDhall.Test.Parser where

import DzenDhall.Config
import DzenDhall.Data
import DzenDhall.Parser
import Test.Tasty (TestTree, TestName, testGroup)
import Test.Tasty.HUnit
import Text.Parsec

mkTest :: TestName -> [Token] -> Either ParseError (Bar SourceSettings) -> TestTree
mkTest name tokenList expected =
  Test.Tasty.HUnit.testCase name $
    runParser DzenDhall.Parser.bar () name tokenList @?= expected

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
                                , escapeMode = EscapeMode True True
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
                               , escapeMode = EscapeMode True True
                               })
      ]
    ]
  ]
