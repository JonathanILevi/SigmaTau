module Control.FRP.Pull (Pull(..)) where

class Pull p where
	grab :: p a -> IO a

