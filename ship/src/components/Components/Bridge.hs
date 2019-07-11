module Components.Bridge (newBridge) where

import Data.List (foldl')
import Control.Monad (void)

import Msg.Bridge.Up
import Msg.Bridge.Down

newBridge :: IO (BridgeUpMsg->IO())
newBridge = do
	return (\m->print m>>return ())

