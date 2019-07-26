module Control.Concurrent.MMap where

import Control.Concurrent.MVar
import Data.Map (Map)
import qualified Data.Map.Strict as M

newtype MMap k a = MMap (Map k a)

newEmpty :: IO (MMap k a)
newEmpty = newMVar M.empty

newSingleton :: k -> a -> IO (MMap k a)
newSingleton k x = newMVar $ M.singleton k x

insert :: Ord k => k -> a -> MMap k a -> IO (Map k a)
insert k x m = readMVar m >>= insert k x >>= writeMVar m

insert :: Ord k => k -> a -> MMap k a -> IO (Map k a)
insert k x m = readMVar m >>= insert k x >>= writeMVar m

