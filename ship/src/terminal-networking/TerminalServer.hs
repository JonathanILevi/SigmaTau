{-# LANGUAGE OverloadedStrings #-}
module TerminalServer (runTerminalServer) where

import Control.Concurrent.STM.TChan
import Data.Text

import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Application.Static
import Network.Wai.Handler.WebSockets
import Network.WebSockets

import Data.FRP.Push as FRP

import Msg.Up
import Msg.Down

import Terminal

runTerminalServer :: Push (Push DownMsg, LackingUpMsg) -> IO ()
runTerminalServer upMsgPush = do
	let port = 8951
	putStrLn $ "Listening on port " ++ show port
	let app = staticApp $ defaultWebAppSettings "www"
	let wsApp = websocketsOr defaultConnectionOptions (\pc->acceptRequest pc >>= runTerminal upMsgPush) app
	run port wsApp
	return ()











