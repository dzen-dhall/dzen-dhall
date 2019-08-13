-- | Functions for `config.dhall` validation.
module DzenDhall.Validation where

import           DzenDhall.Config
import           DzenDhall.Event
import           DzenDhall.Extra

import qualified Text.Megaparsec
import qualified Data.Text
import           Data.Text (Text)
import           Data.Void


type ParseError = Text.Megaparsec.ParseErrorBundle String Void


data Error
  = InvalidSlotAddress ParseError Text
  | InvalidAutomatonAddress ParseError Text


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


-- | Check if the input is parseable by the parser.
getError :: Text.Megaparsec.Parsec Void String Text -> String -> Maybe (Text.Megaparsec.ParseErrorBundle String Void)
getError parser =
  leftToJust . Text.Megaparsec.parse (parser *> Text.Megaparsec.eof) ""


-- | Pretty-print errors.
report :: [Error] -> Text
report [] = "No errors."
report errors = mappend header $ foldMap ((<> "\n\n") . reportError) errors
  where
    header = "Some errors encountered when trying to read the configuration:\n\n\n"
    reportError :: Error -> Text
    reportError = \case
      InvalidSlotAddress err address ->
        "Invalid slot address: " <> address <> "\n" <>
        "Error: " <> Data.Text.pack (Text.Megaparsec.errorBundlePretty err)
      InvalidAutomatonAddress err address ->
        "Invalid automaton address: " <> address <> "\n" <>
        "Error: " <> Data.Text.pack (Text.Megaparsec.errorBundlePretty err)
