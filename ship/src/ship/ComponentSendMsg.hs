module ComponentSendMsg (ComponentSendMsg(..)) where

import Msg.Bridge.Up
import Msg.Thruster.Up

data ComponentSendMsg	= BridgeSendMsg (BridgeUpMsg->IO())
	| ThrusterSendMsg (ThrusterUpMsg->IO())

