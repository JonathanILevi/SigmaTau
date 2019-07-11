module Msg.Up where

import ComponentID
import ComponentType
import Msg.ComponentUp

data UpMsg	= UpMsg ComponentID ComponentUpMsg
	deriving (Show)
data LackingUpMsg = LackingUpMsg ComponentID (ComponentType -> Maybe UpMsg)--Will result in `Nothing` in the case of a deserialize error

makeMsgLacking :: UpMsg -> LackingUpMsg
makeMsgLacking msg@(UpMsg id _) = LackingUpMsg id (\_->Just msg)
