module Data.FRP.LifetimePush (LifetimePush(..), newLifetimePush, makeLifetimePush, push, lifetime) where

import Data.FRP.Lifetime
import Data.FRP.Push

data LifetimePush a = LifetimePush (Push a) (Lifetime)

newLifetimePush :: Push a -> Lifetime -> LifetimePush a
newLifetimePush p l = LifetimePush p l

makeLifetimePush :: (a->IO()) -> IO() -> LifetimePush a
makeLifetimePush pc ec = newLifetimePush (newPush pc) (newLifetime ec) 

push :: LifetimePush a -> Push a
push (LifetimePush p _) = p

lifetime :: LifetimePush a -> Lifetime
lifetime (LifetimePush _ l) = l

