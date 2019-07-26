module Control.FRP.Splitable (Splitable(..)) where

class Splitable s where
	split :: s -> s -> s

