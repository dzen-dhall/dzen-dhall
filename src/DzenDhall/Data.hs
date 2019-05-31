module DzenDhall.Data where

import GHC.Generics
import Data.Text (Text)
import Data.Data
import Data.IORef
import Data.Time.Clock.POSIX

type Color = ()

type Token = ()
type Position = ()

data SourceHandle
  = SourceHandle
  { output :: IORef Text
  , updateInterval :: POSIXTime
  , lastUpdate :: IORef POSIXTime
  }

data Plugin ref
  = Raw Text
  | Source ref Text
  | Txt Text
  | Marquee Integer (Plugin ref)
  | Color Text (Plugin ref)
  | Plugins [Plugin ref]
  deriving (Show, Eq, Generic, Data, Typeable)

initialize :: Plugin () -> IO (Plugin SourceHandle)
initialize (Raw text) = pure $ Raw text
initialize (Source () text) = do
  output <- newIORef ""
  let updateInterval = 10 :: POSIXTime
  currentTime <- getPOSIXTime
  lastUpdate <- newIORef currentTime
  pure $ Source (SourceHandle {..}) text
initialize (Txt text) = pure $ Txt text
initialize (Marquee i p) = Marquee i <$> initialize p
initialize (Color color p) = Color color <$> initialize p
initialize (Plugins ps) = Plugins <$> mapM initialize ps

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
