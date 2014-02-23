-- Client for the betuol server
-- ----------------------------

module Main where

import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv)
import Data.ByteString.Char8 as B (unpack)
import Control.Monad
import Control.Monad.Trans.State

import Instance
import Event

main :: IO ()
main = do

-- Create our instance via a state monad.
-- Run the client loop for n loops.

    let n = 1
    let is' = execState start (Instance [])
    putStrLn . show . runState (loop n) $ is'

-- Get our server network information, and then create a socket to get events from the server.
-- The port here isn't specified because it doesn't matter.
-- Then begin the main loop with getting events and reacting to them.

    addrInfo <- getAddrInfo (Just defaultHints) (Just "127.0.0.1") (Just "13560")
    let serverAddr = head addrInfo
    sock <- socket (addrFamily serverAddr) (addrSocketType serverAddr) (addrProtocol serverAddr)
    connect sock (addrAddress serverAddr)

    getEvent sock
    let evt = RequestCharacterCreationEvent "player" [1,1,1]
    sendEvent sock evt
    getEvent sock
    return ()
