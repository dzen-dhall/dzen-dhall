module DzenDhall where

import           Options.Applicative (execParser)
import           DzenDhall.Arguments (Arguments(..))
import qualified DzenDhall.Arguments as A
import           DzenDhall.Runtime (Runtime(..), mkRuntime)
import           Data.Maybe
import           Control.Monad

main :: IO ()
main = do
  arguments <- execParser A.parserInfo
  runtime <- mkRuntime arguments
  putStrLn "Hello, Haskell!"
