module Main where

import Control.Concurrent

import Control.Concurrent.STM
import Control.Concurrent.STM.TChan

import Data.FRP.Push

import Ship
import TerminalServer


main = do
	msgChan <- atomically $ newTChan
	forkIO $ runTerminalServer (newPush (\m->atomically $ writeTChan msgChan m))
	newShip msgChan
	

