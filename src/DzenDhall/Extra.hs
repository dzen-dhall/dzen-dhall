module DzenDhall.Extra where

import           Control.Monad
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Except
import qualified Data.List
import           Data.Maybe (catMaybes)
import qualified Data.Text
import           Data.Text (Text)
import           Time.Types
import           System.Directory (findExecutable)
import           Control.Concurrent.MVar
import           Foreign.StablePtr


nonNegative :: (Num a, Ord a) => a -> a
nonNegative x
  | x < 0 = 0
  | otherwise = x

positive :: Int -> Int
positive x
  | x < 1 = 1
  | otherwise = x

spaces :: Int -> Text
spaces w = Data.Text.justifyRight w ' ' ""

showPack :: Show a => a -> Text
showPack = Data.Text.pack . show

loopWhileM :: Monad m => m Bool -> m () -> m ()
loopWhileM pr act = do
    b <- pr
    when b do
      act
      loopWhileM pr act

whenJust :: (Monad m, Monoid b) => Maybe a -> (a -> m b) -> m b
whenJust = flip $ maybe (return mempty)

leftToJust :: Either a b -> Maybe a
leftToJust (Left a) = Just a
leftToJust _ = Nothing

withMaybe :: Maybe a -> b -> (a -> b) -> b
withMaybe mb b f = maybe b f mb

withEither :: Either a b -> (a -> c) -> (b -> c) -> c
withEither ei l r = either l r ei

whenLeft :: (Monad m, Monoid b) => Either a b -> (a -> m b) -> m b
whenLeft e f = whenJust (leftToJust e) f

safeHead :: [a] -> Maybe a
safeHead = fmap fst . Data.List.uncons

safeTail :: [a] -> Maybe [a]
safeTail = fmap snd . Data.List.uncons

fromLines :: [Text] -> Text
fromLines = Data.Text.intercalate "\n"

hush :: Either e a -> Maybe a
hush (Left _) = Nothing
hush (Right a) = Just a

throwMaybe :: Monad m => MaybeT m a
throwMaybe = exceptToMaybeT (throwE ())

-- This is a workaround,
-- see https://github.com/vincenthz/hs-hourglass/issues/32
addElapsedP :: ElapsedP -> ElapsedP -> ElapsedP
addElapsedP (ElapsedP e1 (NanoSeconds ns1)) (ElapsedP e2 (NanoSeconds ns2)) =
    let notNormalizedNS = ns1 + ns2
        (retainedNS, ns) = notNormalizedNS `divMod` 1000000000
    in  ElapsedP (e1 + e2 + (Elapsed $ Seconds retainedNS)) (NanoSeconds ns)

-- | Returns a list of executables that are not present in PATH.
checkExecutables :: [String] -> IO [String]
checkExecutables executables = do
  fmap catMaybes $ forM executables \executable ->
    maybe (Just executable) (const Nothing) <$> findExecutable executable

isYes :: String -> Bool
isYes response = response `elem` ["Y", "y", "Yes", "yes", ""]

waitForever :: IO ()
waitForever = do
  m <- newEmptyMVar
  _ <- newStablePtr m
  takeMVar m
