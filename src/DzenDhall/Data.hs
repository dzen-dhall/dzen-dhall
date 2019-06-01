module DzenDhall.Data where

import Data.Data
import Data.IORef
import Data.Text (Text)
import DzenDhall.Config
import GHC.Generics
import Control.Concurrent
import qualified Data.Text

type Color = Text

data SourceHandle
  = SourceHandle
  { outputRef :: IORef Text
  , cacheRef :: Cache
  , threadId :: ThreadId
  }

type Cache = IORef (Maybe Text)

type ParsedPlugin = Plugin SourceSettings

type InitializedPlugin = Plugin SourceHandle

data Plugin ref
  = Raw Text
  | Source ref
  | Txt Text
  | Marquee Integer (Plugin ref)
  | Color Text (Plugin ref)
  | Plugins [Plugin ref]
  deriving (Show, Eq, Generic, Data, Typeable)

data Property
  = BG Color
  | IB
  | FG Color
  | CA (Event, Handler)
  | P Position
  deriving (Eq, Show)

type Event = Text
type Handler = Text
type ImagePath = Text

data Shape
  = I ImagePath
  | R Int Int
  | RO Int Int
  | C Int
  | CO Int
  | Padding
  deriving (Eq, Show)

{- | From dzen docs:

@
^p(+-X)          move X pixels to the right or left of the current position (on the X axis)
^p(+-X;+-Y)      move X pixels to the right or left and Y pixels up or down of the current
                 position (on the X and Y axis)
^p(;+-Y)         move Y pixels up or down of the current position (on the Y axis)
^p()             without parameters resets the Y position to its default
@ -}

{- | @
^p(+-X)          move X pixels to the right or left of the current position (on the X axis)
^p(+-X;+-Y)      move X pixels to the right or left and Y pixels up or down of the current
                 position (on the X and Y axis)
^p(;+-Y)         move Y pixels up or down of the current position (on the Y axis)
^p()             without parameters resets the Y position to its default

Further ^p() also takes some symbolic names as argument:

_LOCK_X          Lock the current X position, useful if you want to
                 align things vertically
_UNLOCK_X        Unlock the X position
_LEFT            Move current x-position to the left edge
_RIGHT           Move current x-position to the right edge
_TOP             Move current y-position to the top edge
_CENTER          Move current x-position to center of the window
_BOTTOM          Move current y-position to the bottom edge
@ -}
data Position =
  -- | 1-4
  XY (Int, Int) |
  -- | Reset the Y position to it's defaults, equivalent to ^p().
  ResetY |
  -- | _LOCK_X          Lock the current X position, useful if you want to align things vertically
  P_LOCK_X |
  -- | _UNLOCK_X        Unlock the X position
  P_UNLOCK_X |
  -- | _LEFT            Move current x-position to the left edge
  P_LEFT |
  -- | _RIGHT           Move current x-position to the right edge
  P_RIGHT |
  -- | _TOP             Move current y-position to the top edge
  P_TOP |
  -- | _CENTER          Move current x-position to the center of the window
  P_CENTER |
  -- | _BOTTOM          Move current y-position to the bottom edge
  P_BOTTOM
  deriving (Eq, Show)

data AST =
  -- | Text.
  ASTText Text |
  -- | Branching
  ASTs AST AST |
  -- | Some property that does not change the size
  Prop Property AST |
  -- | Some shape (@^r, ^i, ^co, etc.@) together with its size in characters
  Container Shape Int |
  -- | @mempty@
  EmptyAST
  deriving (Eq, Show)

instance Semigroup AST where
  (<>) = ASTs

instance Monoid AST where
  mempty = EmptyAST
  mappend = (<>)
  mconcat [] = EmptyAST
  mconcat [x] = x
  mconcat (x:xs) = ASTs x (mconcat xs)

-- | Represents (possibly partial) result of 'splitAST' computation.
data Split a
  = EmptyL a -- ^ a split with empty LHS tree.
  | EmptyR a Int -- ^ a split that has no RHS tree.
  | Twain a a Int -- ^ a split with left side's length guaranteed to be equal to the given number (third constructor's argument).
  deriving (Show, Eq, Functor)

-- | Get progress (width of LHS) of a 'Split', @O(1)@.
getProgress :: Split a -> Int
getProgress (EmptyR _ n) = n
getProgress (Twain _ _ n) = n
getProgress (EmptyL _) = 0

-- | Join results of two (maybe partial) splits.
--
-- Example:
-- @
-- -- read as "incomplete split with empty RHS and some LHS l of length n
-- -- joined together with a complete split with LHS 'a' of length n' and some
-- -- RHS 'b' is a complete split with LHS equal to (l <> a) with length (n + n')
-- -- and RHS equal to 'b'".
-- EmptyR l n =>> Twain a b n'
--   = Twain (l <> a) b (n + n')
-- @
(=>>) :: Semigroup a => Split a -> Split a -> Split a
EmptyL l =>> EmptyL r
  = EmptyL (l <> r)
EmptyL l =>> EmptyR l' _
  = EmptyL (l <> l')
EmptyL l =>> Twain a b _
  = EmptyL (l <> a <> b)

EmptyR l n =>> EmptyL r
  = Twain l r n
EmptyR l n =>> EmptyR r n'
  = EmptyR (l <> r) (n + n')
EmptyR l n =>> Twain a b n'
  = Twain (l <> a) b (n + n')

Twain l r n =>> EmptyL r'
  = Twain l (r <> r') n
Twain l r n =>> EmptyR r' _
  = Twain l (r <> r') n
Twain l r n =>> Twain l' r' _
  = Twain l (r <> l' <> r') n

-- | Like 'Data.List.splitAt', but for 'AST's.
split :: Int -> AST -> (AST, AST)
split n ast = case splitAST n ast of
  EmptyR l _ -> (l, mempty)
  EmptyL r   -> (mempty, r)
  Twain a b _ -> (a, b)

-- | Split tree at the given position.
splitAST :: Int -> AST -> Split AST
splitAST 0 ast        = EmptyL ast
splitAST _ EmptyAST   = EmptyR EmptyAST 0
splitAST n t@(ASTText text)
  | l > n     = Twain (ASTText (Data.Text.take n text))
                      (ASTText (Data.Text.drop n text))
                      n
  | otherwise = EmptyR t l
  where l = Data.Text.length text

splitAST n (ASTs l r) =
  let res = splitAST n l in
    res =>> splitAST (n - getProgress res) r
splitAST n (Prop c t) = fmap (Prop c) (splitAST n t)
splitAST n res@(Container _ l)
  | l > n = Twain res (ASTText $ spaces (l - n)) n
  | otherwise = EmptyR res l
  where
    spaces :: Int -> Text
    spaces n = Data.Text.justifyRight n ' ' ""
