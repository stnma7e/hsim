-- Client for the betuol server
-- ----------------------------

module Main where

import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv)
import Data.ByteString.Char8 as B (unpack)
import Control.Monad
import Data.Maybe
import Control.Monad.Trans
import Control.Monad.Trans.State
import qualified Numeric.Matrix as Mat
import qualified Data.Map as Map

import Instance
import Event
import Math
import Component.Manager.Transform
import Component.Manager.Character

import Event.Attack
import Event.CharacterMoved
import Event.RequestCharacterCreation
import Event.ApproveCharacterCreationRequest

main :: IO ()
main = do

-- Create our instance via a state monad.
-- Run the client loop for n loops.

    let (ret, is) = flip runState emptyInstanceState $ do
        start
        update
--
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
        (Left err)   -> putStrLn err
        (Right evt'@(EventDescriptor typ evtData)) -> do
            dispatch evt'
            loop sock $ execState (reactEvent evt') is

loop :: Socket -> InstanceState -> IO ()
loop sock is = do
    com <- getLine
    let (ret, is'@(InstanceState pl (TransformManager mats) _)) = runState (parseInput com) is
    case ret of 
        (Right "quit") -> return ()
        otherwise -> do
            case ret of
                (Left err)      -> print err
                (Right "show")  -> print is'
                (Right "m")     -> let (Just mat) = Map.lookup pl mats
                                   in sendEvent sock $ CharacterMovedEvent pl
                                       [mat `Mat.at` (1,4), mat `Mat.at` (2,4), mat `Mat.at` (3,4)]
                otherwise       -> return ()

            loop sock $ execState update is'

parseInput :: String -> Instance (Either String String)
parseInput line = do
    let args = words line
    if null args
    then state $ \s -> (Left "no text printed", s)
    else let com = head args
         in state $ \s@(InstanceState pl
                        tm@(TransformManager mats)
                        cm@(CharacterManager ids)) ->
        case com of
            -- creates an object on the client
            -- takes 1 argument of ID to give object
            "create"  -> if length args < 2
                         then (Left "not enough arguments for `create` command", s)
                         else let n = read $ args !! 1
                                  is = execState (createObject n) s
                              in (Right com, is)
            -- movement command
            -- takes 1 argument of direction to move
            "m"       -> if length args < 2
                         then (Left "not enough arguments for `m` command", s)
                         else let (Just mat) = Map.lookup pl mats
                                  direction = case args !! 1 of
                                      "n" -> [ 0, 0, 1]
                                      "s" -> [ 0, 0,-1]
                                      "e" -> [ 1, 0, 0]
                                      "w" -> [-1, 0, 0]
                                  is = execState (moveObject pl (mat `Mat.times` buildTranslationMatrix (4,4) direction)) s
                              in (Right com, is)
            -- attack command
            -- takes 1 argument of ID for player to attack
            "a"       -> if length args < 2
                         then (Left "not enough arguments for `m` command", s)
                         else let n = read $ args !! 1
                                  is = execState (attackObject pl n) s
                              in (Right com, is)
            -- for commands that need to use IO
            -- this block will just let specified commands fall through
            -- to be evaluated in the calling function with access to IO
            otherwise -> if head args `elem` commands
                         then (Right com, s)
                         else (Left "not a command", s)
                             where commands :: [String]
                                   commands = ["quit", "show"]

dispatch :: EventDescriptor -> IO ()
dispatch evt@(EventDescriptor typ evtData)
    | typ == "attack"                          = print (getEvent evt :: AttackEvent)
    | typ == "characterMoved"                  = print (getEvent evt :: CharacterMovedEvent)
    | typ == "approveCharacterCreationRequest" = print (getEvent evt :: ApproveCharacterCreationRequestEvent)
    | otherwise                                = print evt

reactEvent :: EventDescriptor -> Instance ()
reactEvent evt@(EventDescriptor typ evtData) =
    state $ \s@(InstanceState pl tm@(TransformManager mats) cm@(CharacterManager ids)) ->
    case typ of
        "attack" ->
            let (AttackEvent (id1,id2)) = getEvent evt
            in ((), execState (attackObject id1 id2) s)
        "characterMoved" ->
            let (CharacterMovedEvent id loc) = getEvent evt
                mat = Map.lookup id mats
                mat' = case mat of
                    (Just mat'') -> mat''
                    -- if we don't already have any information for this object, then make a new one and update it
                    -- will need to poll the server for data on this object since we don't have it yet
                    -- type information, etc.
                    Nothing -> let newId = execState (createObject id) s
                                   (Just mat'') = Map.lookup id mats
                               in mat''
            in ((), execState (moveObject id (mat' `Mat.times` Mat.fromList [loc])) s)
        "approveCharacterCreationRequest" ->
            let (ApproveCharacterCreationRequestEvent id) = getEvent evt
            in ((), execState (createObject id) s)
        otherwise -> error $ "unsupported event type: "++typ ++ "\n\t data: " ++ show evtData
