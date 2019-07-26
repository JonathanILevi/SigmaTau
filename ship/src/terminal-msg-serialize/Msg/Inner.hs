module Msg.Inner (unserializeUpMsgM, serializeDownMsgM) where

import Data.Either.Combinators (rightToMaybe)
import Data.ByteString

import Data.Serialize.Get
import Data.Serialize.Put
import Data.Serialize.IEEE754

import ComponentID
import ComponentType
import Msg.Up
import Msg.Down
import Msg.ComponentUp	as CU
import Msg.ComponentDown	as CD
import Msg.Bridge.Up	as BU
import Msg.Bridge.Down	as BD
import Msg.Thruster.Up	as TU
import Msg.Thruster.Down	as TD


unserializeUpMsgM :: Get LackingUpMsg
unserializeUpMsgM = do
	cID <- ComponentID <$> fromIntegral <$> getWord8
	r <- getBytes =<< remaining
	return $ LackingUpMsg cID $ (.) (UpMsg cID <$>) ((flip unserializeUpMsgBody) r)

unserializeUpMsgBody :: ComponentType -> ByteString -> Maybe ComponentUpMsg
unserializeUpMsgBody t = (.)rightToMaybe $ runGet $ unserializeUpMsgBody2 t

unserializeUpMsgBodyM :: ComponentType -> Get ComponentUpMsg
unserializeUpMsgBodyM (TypeBridge) = do
	return $ BridgeUpMsg $ BU.Connect
unserializeUpMsgBody2 (TypeThruster) = do
	return $ ThrusterUpMsg $ TU.Set 5

serializeDownMsgM :: DownMsg -> Put
serializeDownMsgM (DownMsg (ComponentID cID) msg) = do
	putWord8 $ fromIntegral cID
	serializeDownMsgBodyM msg 

serializeDownMsgBodyM :: ComponentDownMsg -> Put
serializeDownMsgBodyM (BridgeDownMsg (BD.Add (ComponentID id) t)) = do
	putWord8 $ 0
	putWord8 $ fromIntegral id
	putWord8 $ typeNum t
serializeDownMsgBodyM (BridgeDownMsg (BD.Remove (ComponentID id))) = do
	putWord8 $ 1
	putWord8 $ fromIntegral id

serializeDownMsgBodyM (ThrusterDownMsg (TD.Update value)) = do
	putWord8	$ 0
	putFloat32be	$ value

typeNum :: Integral a => ComponentType -> a
typeNum TypeBridge = 0
typeNum TypeThruster = 1

{-
(f ComponentUpMsg -> f UpMsg)
-> (ComponentType -> Maybe ComponentUpMsg)
-> (ComponentType -> Maybe UpMsg)
-}

----getUpMsg
----
----getComponentUpMsg :: ComponentType -> 
----getComponentUpMsg
----
----instance Serialize DownMsg where
----	get = undefined
----	put (DownMsg (ComponentID id) msg) = do
----		put id
----		put msg
----instance Serialize UpMsg where
----	get = UpMsg <$> get <*> get
----	put _ = undefined
----
----instance Serialize ComponentDownMsg where
----	get = undefined
----	put (BridgeDownMsg msg) = put msg
----	put (ThrusterDownMsg msg) = put msg
----instance Serialize ComponentUpMsg where
----	get = ComponentUpMsg <$> get <*> get
----	put _ = undefined
----
----
----instance Serialize BridgeUpMsg where
----	get = undefined
----	put _ = undefined
----instance Serialize ThrusterDownMsg where
----	get = do
----		t <- getWord8
----		return $ case t of 
----			0 -> TU.Update <$> (get :: Get Float)
----	put _ = undefined
----
----instance Serialize BridgeDownMsg where
----	get = undefined
----	put (BD.Add) = do
----		putWord8 0
----	put (BD.Remove) = do
----		putWord8 1
----instance Serialize ThrusterUpMsg where
----	get = undefined
----	put (TU.Stream) = do
----		putWord8 0
----	put (TU.Set v) = do
----		putWord8 1
----		put v
 
