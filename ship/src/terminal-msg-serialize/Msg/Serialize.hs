module Msg.Serialize (unserializeUpMsg, serializeDownMsg) where

import Data.Either.Combinators (rightToMaybe)
import Data.ByteString

import Data.Serialize.Get
import Data.Serialize.Put

import Msg.Up
import Msg.Down

import qualified Msg.Inner as I


unserializeUpMsg :: ByteString -> Maybe LackingUpMsg
unserializeUpMsg = (.)rightToMaybe $ runGet $ I.unserializeUpMsgM

serializeDownMsg :: DownMsg -> ByteString
serializeDownMsg msg = runPut $ I.serializeDownMsgM msg
