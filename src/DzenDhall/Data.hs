{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module DzenDhall.Data where

import qualified Data.HashMap.Strict as H
import           Data.IORef
import qualified Data.Text
import           Data.Text (Text)
import           Data.Vector
import           DzenDhall.Config
import           GHC.Generics
import           Lens.Micro.TH
import           Data.Void

type Cache = IORef (Maybe Text)

data SourceHandle
  = SourceHandle
  { _shOutputRef :: IORef Text
  , _shCacheRef :: Cache
  , _shEscapeMode :: EscapeMode
  }

makeLenses ''SourceHandle

data Bar id
  = BarRaw Text
  | BarSource (SourceRefX id)
  | BarText Text
  | BarMarquee Marquee (Bar id)
  | BarSlider Slider (Vector (Bar id))
  | BarAutomaton Text (StateTransitionTableX id) (AutomataRefX id (Bar id))
  | BarListener Text (Bar id)
  | BarScope (Bar id)
  | BarProp Property (Bar id)
  | Bars [Bar id]
  deriving (Generic)

type family AutomataRefX          id :: * -> *
type family StateTransitionTableX id
type family SourceRefX            id

newtype Initialized = Initialized Void
newtype Marshalled  = Marshalled  Void

type instance AutomataRefX          Marshalled = H.HashMap Text
type instance StateTransitionTableX Marshalled = StateTransitionTable
type instance SourceRefX            Marshalled = Source

type instance AutomataRefX          Initialized = IORef
type instance StateTransitionTableX Initialized = ()
type instance SourceRefX            Initialized = SourceHandle

deriving instance Show (Bar Marshalled)
deriving instance Eq   (Bar Marshalled)

instance Semigroup (Bar id) where
  a <> b = Bars [a, b]

instance Monoid (Bar id) where
  mempty = Bars []

data Property
  = BG Color
  | IB
  | FG Color
  | CA ClickableArea
  | P Position
  | PA AbsolutePosition
  deriving (Eq, Show)

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

data AST =
  -- | Text.
  ASTText Text |
  -- | Branching
  ASTs AST AST |
  -- | Some property that does not change the visible size of the inner AST.
  Prop Property AST |
  -- | Some shape (@^r, ^i, ^co, etc.@) together with its size in characters.
  Container Shape Int |
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
    spaces w = Data.Text.justifyRight w ' ' ""

astWidth :: AST -> Int
astWidth (ASTText txt) = Data.Text.length txt
astWidth (ASTs a b) = astWidth a + astWidth b
astWidth (Prop _ a) = astWidth a
astWidth (Container _ w) = w
astWidth EmptyAST = 0
