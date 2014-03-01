-- Client for the betuol server
-- ----------------------------

module Main where

import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv)
import Data.ByteString.Char8 as B (unpack)
import Control.Monad
import Data.Maybe
import Control.Monad.Trans.State

import Instance
import Event

import Event.Attack
import Event.CharacterMoved
import Event.RequestCharacterCreation
import Event.ApproveCharacterCreationRequest

main :: IO ()
main = do

-- Get our server network information, and then create a socket to get events from the server.
-- The port here isn't specified because it doesn't matter.
-- Then begin the main loop with getting events and reacting to them.

    addrInfo <- getAddrInfo (Just defaultHints) (Just "127.0.0.1") (Just "13560")
    let serverAddr = head addrInfo
    sock <- socket (addrFamily serverAddr) (addrSocketType serverAddr) (addrProtocol serverAddr)
    connect sock (addrAddress serverAddr)

    sendEvent sock $ RequestCharacterCreationEvent "player" [1,1,1]
    evt <- recvEvent sock
    case evt of
        (Right evt') -> dispatch evt'
        (Left err)   -> putStrLn err

-- Create our instance via a state monad.
-- Run the client loop for n loops.

    let n = 1
    let (ret, is) = (flip runState) (InstanceState []) $ do
        start
        loop n

    processConsole is

processConsole :: InstanceState -> IO ()
processConsole is = do
    com <- getLine
    let (ret, is') = runState (parseInput com) is
    case ret of 
        (Right "quit") -> return ()
        otherwise -> do
            case ret of
                (Left err)     -> print err
                (Right "show") -> print is'
                otherwise      -> return ()

            processConsole is'

parseInput :: String -> Instance (Either String String)
parseInput com = do
    let args = words com
    if length args == 0
    then state $ \s -> (Left "no text printed", s)
    else state $ \s -> case head args of
        "create"  -> if length args < 2
                     then (Left "not enough arguments to `create` command", s)
                     else let n = read $ args !! 1
                              is = execState (createObject n) s
                          in (Right com, is)
        "quit"    -> (Right com, s)
        "show"    -> (Right com, s)
        otherwise -> (Left "not a command", s)

dispatch :: EventDescriptor -> IO ()
dispatch evt@(EventDescriptor typ evtData)
    | typ == "attack"                          = print (getEvent evt :: AttackEvent)
    | typ == "characterMoved"                  = print (getEvent evt :: CharacterMovedEvent)
    | typ == "approveCharacterCreationRequest" = print (getEvent evt :: ApproveCharacterCreationRequestEvent)
    | otherwise                                = print evt
