module Components.Bridge (newBridge) where

import Data.List (foldl')

import Control.Concurrent.STM (atomically)
import Data.Map.Strict as M

import Data.FRP.Pull
import Data.FRP.Push
import Data.FRP.ConstMap
import Data.FRP.ConstPushSet
import Data.FRP.Lifetime

import ComponentID
import ComponentType

import Component

import Msg.Bridge.Up
import Msg.Bridge.Down

type ComponentRef = (ComponentID, ComponentType)

newBridge :: IO (Push (Push BridgeDownMsg, BridgeUpMsg), ConstPushSet ComponentRef)
newBridge = do
	(ConstMap componentsPushSet2 componentsPull) <- newConstMap
	let componentsPushSet = newConstPushSet (\v->print v>>add componentsPushSet2 v)
	
	let	onMsg (sender, Connect) = do
			putStrLn "ConnectMsg"
			components <- grab componentsPull
			sequence_ $ (flip fmap) (M.assocs components) $ \(id,t)->do
				if id==ComponentID 0 then return ()
					else send sender $ Add id t
	----componentMap <- atomically M.new
	----let pushSet = newConstPushSet (\(id,t)->do
	----		putStrLn "componentAdded"
	----		print id
	----		atomically (M.insert t id componentMap)
	----		return $ newLifetime $ putStrLn "componentRemoved" >> print id >> (atomically $ M.delete id componentMap)
	----	)
	return (newPush onMsg, componentsPushSet)

	