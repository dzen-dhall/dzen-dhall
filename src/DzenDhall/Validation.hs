-- | Functions for `config.dhall` validation.
module DzenDhall.Validation where

import           DzenDhall.Config
import           DzenDhall.Event
import           DzenDhall.Extra

import           Data.Maybe
import           Data.Text (Text)
import           Data.Void
import           Lens.Micro
import           System.Directory (findExecutable)
import           System.Exit
import           System.Process
import qualified Data.Text
import qualified Text.Megaparsec


type ParseError = Text.Megaparsec.ParseErrorBundle String Void


data Error
  = InvalidSlotAddress ParseError Text
  | InvalidAutomatonAddress ParseError Text
  | BinaryNotInPath Text Text
  | AssertionFailure Text Text


run :: [Token] -> IO ([Error], [Token])
run tokens = do
  let errors = validate tokens
  assertionErrors <- checkAssertions tokens
  pure $ (errors <> assertionErrors, filterOutAssertions tokens)
    where
      filterOutAssertions = filter $ \case
        TokAssertion _ -> False
        _              -> True


validate :: [Token] -> [Error]
validate = reverse . go []
  where
    go acc [] = acc
    go acc (TokOpen (OAutomaton address _) : rest) =
      go (proceed InvalidAutomatonAddress automatonAddressParser address acc) rest
    go acc (TokOpen (OListener slot) : rest) =
      go (proceed InvalidSlotAddress slotNameParser slot acc) rest
    go acc (_ : rest) =
      go acc rest

    proceed cont parser what acc =
      case getError parser (Data.Text.unpack what) of
        Nothing -> acc
        Just err -> cont err what : acc

checkAssertions :: [Token] -> IO [Error]
checkAssertions [] = pure []
checkAssertions (TokAssertion assertion : xs) = do
  newErrors <-
    case assertion ^. assCheck of
      BinaryInPath binary -> do
        mbPath <- findExecutable (Data.Text.unpack binary)
        pure [ BinaryNotInPath binary (assertion ^. assMessage) | isNothing mbPath ]
      SuccessfulExit code -> do
        let process = System.Process.shell (Data.Text.unpack code)
        (exitCode, _, _) <- System.Process.readCreateProcessWithExitCode process ""
        pure [ AssertionFailure code (assertion ^. assMessage) | exitCode /= ExitSuccess ]

  (newErrors ++) <$> checkAssertions xs

checkAssertions (_ : xs) = checkAssertions xs

-- | Check if the input is parseable by the parser.
getError :: Text.Megaparsec.Parsec Void String Text -> String -> Maybe (Text.Megaparsec.ParseErrorBundle String Void)
getError parser =
  leftToJust . Text.Megaparsec.parse (parser *> Text.Megaparsec.eof) ""


-- | Pretty-print errors.
report :: [Error] -> Text
report [] = "No errors."
report errors = mappend header $ foldMap ((<> "\n\n") . reportError) errors
  where
    header = "Some errors encountered while trying to read the configuration:\n\n\n"

    namingConventions = "More info: https://github.com/dzen-dhall/dzen-dhall#naming-conventions"

    reportError :: Error -> Text
    reportError = \case
      InvalidSlotAddress err address -> fromLines
        [ "Invalid slot address: " <> address
        , "Error: " <> Data.Text.pack (Text.Megaparsec.errorBundlePretty err)
        , namingConventions
        ]

      InvalidAutomatonAddress err address -> fromLines
        [ "Invalid automaton address: " <> address
        , "Error: " <> Data.Text.pack (Text.Megaparsec.errorBundlePretty err)
        , namingConventions
        ]

      BinaryNotInPath binary message -> fromLines $
        [ "One of required binaries was not found in $PATH: " <> binary
        ] <>
        [ fromLines
          [ ""
          , "Message:"
          , ""
          , message
          ]
        | not (Data.Text.null message)
        ]

      AssertionFailure code message -> fromLines $
        [ "One of assertions failed:"
        , ""
        , code
        ] <>
        [ fromLines
          [ ""
          , "Message:"
          , ""
          , message
          ]
        | not (Data.Text.null message)
        ]
