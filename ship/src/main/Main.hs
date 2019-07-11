module Main where

import Control.Concurrent

import Control.Concurrent.STM
import Control.Concurrent.STM.TChan

import Ship
import TerminalServer


main = do
	msgChan <- atomically $ newTChan
	forkIO $ runTerminalServer msgChan
	newShip msgChan
	

