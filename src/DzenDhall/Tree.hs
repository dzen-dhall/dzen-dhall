module DzenDhall.Tree where
import GHC.Generics
import Data.Text

type Color = ()

type Token = ()
type Position = ()

data Plugin
  = Raw Text
  | Shell Text
  | Txt Text
  | Marquee Integer Plugin
  | Color Text Plugin
  | Plugins [Plugin]
  deriving (Show, Eq, Generic)

data AST =
  -- | Text.
  Text_ String |
  -- | Raw text (no escaping)
  RawText String |
  -- | Raw list of tokens. Use with care - it may break the code!
  RawTokens [Token] |
  -- | Padding. Numeric argument represents width (in characters).
  Padding Int |
  -- | Just branching
  ASTs [AST] |
  -- | Set background color
  BG Color AST |
  -- | Set foreground color
  FG Color AST |
  -- | Image (path, width in characters)
  I String Int |
  -- | Rectangle
  R (Int, Int) |
  -- | Rectangle outline
  RO (Int, Int) |
  -- | Circle
  C Int |
  -- | Circle outline
  CO Int |
  -- | Set relative position
  P Position AST |
  -- | Clickable area
  CA (String, String) AST
  deriving (Show, Eq, Generic)
