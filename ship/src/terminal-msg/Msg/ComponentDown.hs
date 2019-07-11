module Msg.ComponentDown where

import Msg.Bridge.Down
import Msg.Thruster.Down

data ComponentDownMsg	= BridgeDownMsg	BridgeDownMsg
	| ThrusterDownMsg	ThrusterDownMsg
	deriving (Show)

