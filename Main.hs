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

import Event.Attack
import Event.CharacterMoved
import Event.RequestCharacterCreation
import Event.ApproveCharacterCreationRequest

main :: IO ()
main = do

-- Create our instance via a state monad.
-- Run the client loop for n loops.

    let n = 1
    let is' = execState start (Instance [])
    print . runState (loop n) $ is'

-- Get our server network information, and then create a socket to get events from the server.
-- The port here isn't specified because it doesn't matter.
-- Then begin the main loop with getting events and reacting to them.

    addrInfo <- getAddrInfo (Just defaultHints) (Just "127.0.0.1") (Just "13560")
    let serverAddr = head addrInfo
    sock <- socket (addrFamily serverAddr) (addrSocketType serverAddr) (addrProtocol serverAddr)
    connect sock (addrAddress serverAddr)

    sendEvent sock $ RequestCharacterCreationEvent "player" [1,1,1]
    evt <- recvEvent sock
    dispatch evt

dispatch :: EventDescriptor -> IO ()
dispatch evt@(EventDescriptor typ evtData)
    | typ == "attack"                          = print (getEvent evt :: AttackEvent)
    | typ == "characterMoved"                  = print (getEvent evt :: CharacterMovedEvent)
    | typ == "approveCharacterCreationRequest" = print (getEvent evt :: ApproveCharacterCreationRequestEvent)
    | otherwise                                = print evt
