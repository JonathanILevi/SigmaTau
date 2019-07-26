module Components.Thruster (newThruster) where

import Data.List (foldl')
import Control.Monad (void)

import Data.FRP.Push

import Msg.Thruster.Up
import Msg.Thruster.Down

newThruster :: IO (Push (Push ThrusterDownMsg, ThrusterUpMsg))
newThruster = do
	return (newPush (\(t, m)->print m>>return ()))

