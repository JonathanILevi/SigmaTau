module Ship (newShip) where

import Control.Monad (forever)
import Control.Concurrent (threadDelay)
import Data.Maybe

import Control.Concurrent.Chan
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan

import ComponentID
import ComponentType

import Msg.Up
import Msg.Down

type TerminalSendChan = Chan DownMsg

newShip :: TChan (TerminalSendChan, LackingUpMsg) -> IO ()
newShip incommingMsgs = do
	forever $ do
		msg <- atomically $ tryReadTChan incommingMsgs
		maybe (return ()) (\(_, LackingUpMsg cID f)->do
				if (cID == ComponentID 0)
					then print $ f TypeBridge
					else print $ f TypeThruster
			) msg
		threadDelay 500
	






