module Data.FRP.PushSet (PushSet(..), newPushSet, add, split) where

import Data.Functor.Contravariant

import Control.FRP.Splitable
import Data.FRP.LifetimePush

data PushSet a = PushSet (a -> IO (LifetimePush a))

newPushSet :: (a->IO (LifetimePush a)) -> PushSet a
newPushSet nc = PushSet nc

add :: PushSet a -> a -> IO (LifetimePush a)
add (PushSet f) v = f v
