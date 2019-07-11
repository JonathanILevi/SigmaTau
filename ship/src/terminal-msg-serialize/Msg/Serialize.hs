module Msg.Serialize (unserializeUpMsg, serializeDownMsg) where

import Data.Either.Combinators (rightToMaybe)

import Data.Serialize.Get
import Data.Serialize.Put

import ComponentID
import ComponentType
import Data.ByteString
import Msg.Up
import Msg.Down
import Msg.ComponentUp	as CU
import Msg.ComponentDown	as CD
import Msg.Bridge.Up	as BU
import Msg.Bridge.Down	as BD
import Msg.Thruster.Up	as TU
import Msg.Thruster.Down	as TD


unserializeUpMsg :: ByteString -> Maybe LackingUpMsg
unserializeUpMsg = (.)rightToMaybe $ runGet $ do
	cID <- ComponentID <$> fromIntegral <$> getWord8
	r <- getBytes =<< remaining
	return $ LackingUpMsg cID $ (.) (UpMsg cID <$>) ((flip unserializeUpMsgBody) r)

unserializeUpMsgBody :: ComponentType -> ByteString -> Maybe ComponentUpMsg
unserializeUpMsgBody (TypeBridge) = (.)rightToMaybe $ runGet $ do
	return $ BridgeUpMsg $ BU.Connect
unserializeUpMsgBody (TypeThruster) = (.)rightToMaybe $ runGet $ do
	return $ ThrusterUpMsg $ TU.Set 5

serializeDownMsg :: DownMsg -> ByteString
serializeDownMsg (DownMsg (ComponentID cID) msg) = runPut $ do
	putWord8 $ fromIntegral cID

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
 
