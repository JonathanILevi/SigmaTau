module Data.FRP.ConstPushSet (ConstPushSet(..), newConstPushSet, add) where

import Data.Functor.Contravariant

import Control.FRP.Splitable
import Data.FRP.Lifetime

data ConstPushSet a = ConstPushSet (a -> IO Lifetime)

newConstPushSet :: (a->IO Lifetime) -> ConstPushSet a
newConstPushSet nc = ConstPushSet nc

add :: ConstPushSet a -> a -> IO Lifetime
add (ConstPushSet f) v = f v

instance Splitable (ConstPushSet a) where
	split (ConstPushSet x) (ConstPushSet y) = newConstPushSet	(\v -> do
			xl <- x v
			yl <- y v
			return $ newLifetime $ kill xl>>kill yl
		)
instance Contravariant ConstPushSet where
	contramap f p = newConstPushSet (\v->add p (f v))

