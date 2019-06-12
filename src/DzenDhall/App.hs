module DzenDhall.App where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Lens.Micro

import DzenDhall.Runtime

newtype App a = App { unApp :: StateT Runtime IO a }
  deriving (Functor, Applicative, Monad)

runApp :: Runtime -> App a -> IO a
runApp rt app = evalStateT (unApp app) rt

liftIO :: IO a -> App a
liftIO = App . lift

getRuntime :: App Runtime
getRuntime = App get

putRuntime :: Runtime -> App ()
putRuntime rt = App $ put rt

modifyRuntime :: (Runtime -> Runtime) -> App ()
modifyRuntime f = getRuntime >>= putRuntime . f

getCounter :: App Int
getCounter = do
  rt <- getRuntime
  putRuntime $ rt & rtCounter +~ 1
  pure $ rt ^. rtCounter

mapApp :: (IO a -> IO b) -> (App a -> App b)
mapApp f app = do
  rt <- getRuntime
  liftIO $ f $ runApp rt app
