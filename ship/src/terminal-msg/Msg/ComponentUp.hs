module Msg.ComponentUp where

import Msg.Bridge.Up
import Msg.Thruster.Up

data ComponentUpMsg	= BridgeUpMsg	BridgeUpMsg
	| ThrusterUpMsg	ThrusterUpMsg
	deriving (Show)

