module Components.Bridge (newBridge) where

import Data.List (foldl')
import Control.Monad (void)

import Msg.Bridge.Up
import Msg.Bridge.Down
import ShipMsg.Bridge.To
import ShipMsg.Bridge.From

newBridge :: IO (BridgeUpMsg->IO(),BridgeToMsg->IO())
newBridge = do
	return (\m->print m>>return (),\m->print m>>return ())

