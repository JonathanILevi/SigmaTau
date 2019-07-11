module Components.Thruster (newThruster) where

import Data.List (foldl')
import Control.Monad (void)

import Msg.Thruster.Up
import Msg.Thruster.Down
import ShipMsg.Thruster.To
import ShipMsg.Thruster.From

newThruster :: IO (ThrusterUpMsg->IO(),ThrusterToMsg->IO())
newThruster = do
	return (\m->print m>>return (),\m->print m>>return ())

