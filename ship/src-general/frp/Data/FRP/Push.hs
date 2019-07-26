module Data.FRP.Push (Push(..), newPush, send, split) where

import Data.Functor.Contravariant

import Control.FRP.Push hiding (Push)
import qualified Control.FRP.Push as C
import Control.FRP.Splitable

data Push a = Push (a -> IO ())
	
newPush :: (a -> IO ()) -> Push a
newPush f = Push f

instance C.Push Push where
	send (Push f) v = f v
instance Splitable (Push a) where
	split x y = newPush (\v -> send x v >> send y v >> return ())
instance Contravariant Push where
	contramap f p = newPush (\v->send p (f v))

