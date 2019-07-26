module Component (Component(..), componentType, componentMsgPush) where

import Data.Functor.Contravariant

import Data.FRP.Push
import Data.FRP.ConstPushSet

import ComponentID
import ComponentType

import Msg.ComponentUp
import Msg.ComponentDown
import Msg.Bridge.Up
import Msg.Bridge.Down
import Msg.Thruster.Up
import Msg.Thruster.Down

data Component	= Bridge	(Push (Push BridgeDownMsg, BridgeUpMsg))
	| Thruster 	(Push (Push ThrusterDownMsg, ThrusterUpMsg))	

componentType :: Component -> ComponentType
componentType (Bridge _) = TypeBridge
componentType (Thruster _) = TypeThruster

componentMsgPush :: Component -> Push (Push ComponentDownMsg, ComponentUpMsg)
componentMsgPush (Bridge mPh) = (\(phDM,(BridgeUpMsg um)) -> (BridgeDownMsg >$< phDM, um)) >$< mPh
componentMsgPush (Thruster mPh) = (\(phDM,(ThrusterUpMsg um)) -> (ThrusterDownMsg >$< phDM, um)) >$< mPh


