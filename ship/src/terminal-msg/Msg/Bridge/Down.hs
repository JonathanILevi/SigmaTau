module Msg.Bridge.Down where

import ComponentID
import ComponentType

data BridgeDownMsg	= Add ComponentID ComponentType
	| Remove ComponentID
	deriving (Show)

