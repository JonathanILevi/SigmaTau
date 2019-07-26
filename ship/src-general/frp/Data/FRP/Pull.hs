module Data.FRP.Pull (Pull(..), newPull, grab, cachedPush) where

import Control.Concurrent.MVar
import Control.Monad (void)

import Data.FRP.Push

data Pull a = Pull (IO a)

newPull :: (IO a) -> Pull a
newPull f = Pull f

grab :: Pull a -> IO a
grab (Pull f) = f

cachedPush :: a -> IO (Push a, Pull a)
cachedPush s = do
	var <- newMVar s
	let push = newPush (\v->void $ swapMVar var v)
	let pull = newPull (readMVar var)
	return (push, pull)

