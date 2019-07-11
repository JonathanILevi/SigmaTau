module Components.Thruster (newThruster) where

import Data.List (foldl')
import Control.Monad (void)

import Msg.Thruster.Up
import Msg.Thruster.Down

newThruster :: IO (ThrusterUpMsg->IO())
newThruster = do
	return (\m->print m>>return ())

