-- Client for the betuol server
-- ----------------------------

module Main where

import Network.Socket hiding           (recv)
import Network.Socket.ByteString       (recv)
import Data.ByteString.Char8 as B      (unpack)
import Control.Monad.Trans.State       (state, execState, runState)
import qualified Numeric.Matrix as Mat (at, times, fromList)
import qualified Data.Map as Map       (lookup)

import Instance
import Game
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

-- Get our server network information, and then create a socket to get events from the server.
-- The port here isn't specified because it doesn't matter.
-- Then begin the main loop with getting events and reacting to them.

    -- if you use 'localhost' instead of 127.0.0.1 for the address, then it doesn't work
    addrInfo <- getAddrInfo (Just defaultHints) (Just "127.0.01") (Just "13560")
    let serverAddr = head addrInfo
    sock <- socket (addrFamily serverAddr) (addrSocketType serverAddr) (addrProtocol serverAddr)
    connect sock (addrAddress serverAddr)

    sendEvent sock $ RequestCharacterCreationEvent "player" [1,1,1]
    evt <- recvEvent sock
    case evt of
        (Left err)   -> putStrLn err
        (Right evt'@(EventDescriptor typ evtData)) ->
            -- we're looking for an `approveCharacterCreationRequest' event from the server
            -- to get the player's GOiD
            if typ /= "approveCharacterCreationRequest"
            then error $ "wrong event recieved: should be approveCharacterCreationRequest; was "++typ
            else do
                putStrLn $ dispatch evt'

                -- get player's goid from the server make an Instance with that
                let (ApproveCharacterCreationRequestEvent id)  = getEvent evt' :: ApproveCharacterCreationRequestEvent
                let is = flip execState emptyInstanceState $ do
                    start id
                    update
                isn <- run (Scene1) is
                let is' = execState isn is
                loop sock $ execState (reactEvent evt') is'
                return ()

loop :: Socket -> InstanceState -> IO (Instance ())
loop sock is = do
    com <- getLine
    let (ret, is'@(InstanceState pl (TransformManager mats) _ _)) = runState (parseInput com) is
    case ret of 
        (Right "quit") -> return . state $ \s -> ((), is)
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
    state $ \s@(InstanceState pl tm@(TransformManager mats) cm@(CharacterManager ids) _) ->
        if null args
        then (Left "no text printed", s)
        else let com = head args
            in case com of
            -- creates an object on the client
            -- takes 1 argument of ID to give object
            "create"  -> if length args < 2
                         then (Left "not enough arguments for `create` command", s)
                         else let n = read $ args !! 1
                              in (Right com, execState (createObjectSpecificID n) s)
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
                              in (Right com, execState (moveObject pl (mat `Mat.times` buildTranslationMatrix (4,4) direction)) s)
            -- attack command
            -- takes 1 argument of ID for player to attack
            "a"       -> if length args < 2
                         then (Left "not enough arguments for `a` command", s)
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

dispatch :: EventDescriptor -> String 
dispatch evt@(EventDescriptor typ evtData)
    | typ == "attack"                          = show (getEvent evt :: AttackEvent)
    | typ == "characterMoved"                  = show (getEvent evt :: CharacterMovedEvent)
    | typ == "approveCharacterCreationRequest" = show (getEvent evt :: ApproveCharacterCreationRequestEvent)
    | otherwise                                = show evt

reactEvent :: EventDescriptor -> Instance ()
reactEvent evt@(EventDescriptor typ evtData) =
    state $ \s@(InstanceState pl tm@(TransformManager mats) cm@(CharacterManager ids) _) ->
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
                    Nothing -> let newId = execState (createObjectSpecificID id) s
                                   (Just mat'') = Map.lookup id mats
                               in mat''
            in ((), execState (moveObject id (mat' `Mat.times` Mat.fromList [loc])) s)
        "approveCharacterCreationRequest" ->
            let (ApproveCharacterCreationRequestEvent id) = getEvent evt
            in ((), execState (createObjectSpecificID id) s)
        otherwise -> error $ "unsupported event type: "++typ ++ "\n\t data: " ++ show evtData
