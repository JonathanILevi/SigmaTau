{-# LANGUAGE OverloadedStrings #-}
module Terminal (runTerminal, TerminalSendChan) where

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan

import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Application.Static
import Network.Wai.Handler.WebSockets
import Network.WebSockets

import Data.ByteString.Lazy (fromStrict)

import ComponentID
import ComponentType

import Msg.Up
import Msg.Down
import Msg.ComponentUp
import Msg.ComponentDown
import Msg.Bridge.Up as MBU
import Msg.Serialize

type TerminalSendChan = Chan DownMsg

runTerminal :: TChan (TerminalSendChan, LackingUpMsg) -> Connection -> IO ()
runTerminal upMsgChan connection = do
	putStrLn "New Connection"
	sendChannel <- newChan
	atomically $ writeTChan upMsgChan $ (sendChannel, makeMsgLacking $ UpMsg (ComponentID 0) (BridgeUpMsg MBU.Connect))
	--reading thread
	forkIO $ do
		return ()
	--writing thread
	forkIO $ do
		sequence_ =<< fmap (sendDataMessage connection . Binary . fromStrict . serializeDownMsg) <$> getChanContents sendChannel
	return ()











