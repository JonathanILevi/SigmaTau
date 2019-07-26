{-# LANGUAGE TupleSections #-}
module Ship (newShip) where

import Control.Monad (forever, join)
import Control.Concurrent (threadDelay)
import Data.Maybe

import Data.Functor.Syntax
import Data.Functor.Contravariant

import Control.Concurrent.Chan
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM (atomically)
import StmContainers.Map as SM

import Data.Map hiding (split)
import qualified Data.Map as M

import Data.FRP.Push
import Data.FRP.Pull
import Data.FRP.ConstPushSet
import Data.FRP.ConstMap
import Data.FRP.Lifetime

import ComponentID
import ComponentType
import ComponentSendMsg
import Component
import Components.Bridge
import Components.Thruster

import Msg.Up
import Msg.Down

newShip :: TChan (Push DownMsg, LackingUpMsg) -> IO ()
newShip incommingMsgs = do
	(componentsPush, componentsPull) <- do
		(bridgeMsgPush, bridgeComponents) <- newBridge
		(ConstMap componentsPush componentsPull) <- newConstMap
		let componentsPushToBoth = split	((\(id,c)->(id,componentType c)) >$< bridgeComponents)
			componentsPush
		add componentsPushToBoth (ComponentID 0, Bridge bridgeMsgPush)
		return (componentsPushToBoth, componentsPull)
	add componentsPush . (ComponentID 1,) . Thruster =<< newThruster
	add componentsPush . (ComponentID 2,) . Thruster =<< newThruster
	
	let	getComponentType cID = componentType <$$> (M.lookup cID <$> (grab componentsPull))
		
		resolveMsg (LackingUpMsg cID f) = join <$> f <$$> getComponentType cID
		
		distributeMsg :: (Push DownMsg, LackingUpMsg) -> IO ()
		distributeMsg (downMsgPush, lackingMsg) = do
			maybeMsg <- resolveMsg lackingMsg
			fmap join $ sequenceA $ do
				(UpMsg cID componentMsg) <- maybeMsg
				let componentDownMsgPush = (\m->DownMsg cID m) >$< downMsgPush
				return $ do
					maybeC <- M.lookup cID <$> grab (componentsPull)
					sequenceA $ do
						c <- maybeC
						return $ send (componentMsgPush c) (componentDownMsgPush, componentMsg)
			return ()
	
	forever $ do
		maybeLackingMsg <- atomically $ tryReadTChan incommingMsgs
		sequenceA $ distributeMsg <$> maybeLackingMsg
		maybe (return ()) (\(_, LackingUpMsg cID f)->do
				if (cID == ComponentID 0)
					then print $ f TypeBridge
					else print $ f TypeThruster
			) maybeLackingMsg
		threadDelay 500
	






