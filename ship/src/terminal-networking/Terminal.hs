{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Terminal (runTerminal) where

import Control.Monad (void, forever)

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan

import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Application.Static
import Network.Wai.Handler.WebSockets
import Network.WebSockets

import Data.ByteString.Lazy (fromStrict, toStrict)

import Data.FRP.Push as FRP

import ComponentID
import ComponentType

import Msg.Up
import Msg.Down
import Msg.ComponentUp
import Msg.ComponentDown
import Msg.Bridge.Up as MBU
import Msg.Bridge.Down as MBD
import Msg.Serialize

runTerminal :: Push (Push DownMsg, LackingUpMsg) -> Connection -> IO ()
runTerminal upMsgPush connection = do
	putStrLn "New Connection"
	sendChannel <- newChan
	let sendPush = newPush (\m->print m>>writeChan sendChannel m)
	FRP.send upMsgPush $ (sendPush, makeMsgLacking $ UpMsg (ComponentID 0) (BridgeUpMsg MBU.Connect))
	--reading thread
	forkIO $ do
		forever $ do
			dataMsg <- receiveDataMessage connection
			putStr "recieved: "
			print dataMsg
			case dataMsg of
				(Binary d) -> void $ sequenceA $ FRP.send upMsgPush . (sendPush,) <$> (unserializeUpMsg $ toStrict d)
				(Text _ _) -> return ()
	--writing thread
	sequence_ =<< fmap (sendDataMessage connection . Binary . fromStrict . serializeDownMsg) <$> getChanContents sendChannel











