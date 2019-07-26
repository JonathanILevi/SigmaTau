module Control.FRP.Push (Push(..)) where

class Push p where
	send :: p a -> a -> IO ()

