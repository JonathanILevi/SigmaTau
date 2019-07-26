module Data.FRP.ConstMap (ConstMap(..), newConstMap, constPushSet, pull) where

import Data.Map
import Control.Concurrent.MVar
import Control.Monad (void)

import Data.FRP.ConstPushSet
import Data.FRP.Pull
import Data.FRP.Lifetime

data ConstMap k a = ConstMap (ConstPushSet (k,a)) (Pull (Map k a))

newConstMap :: Ord k => IO (ConstMap k a)
newConstMap = do
	mapVar <- newMVar empty
	let phs = newConstPushSet (\(k,v)->do
			modifyMVar_ mapVar (\m->return $ insert k v m)
			return $ newLifetime $ (modifyMVar_ mapVar (\m->return $ delete k m))
		)
	let pl = newPull (readMVar mapVar)
	return $ ConstMap phs pl

constPushSet :: ConstMap k a -> ConstPushSet (k,a)
constPushSet (ConstMap phs _) = phs

pull :: ConstMap k a -> Pull (Map k a)
pull (ConstMap _ pl) = pl


