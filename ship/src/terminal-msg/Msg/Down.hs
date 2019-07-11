module Msg.Down where

import ComponentID
import Msg.ComponentDown

data DownMsg	= DownMsg ComponentID ComponentDownMsg
	deriving (Show)

