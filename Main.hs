-- Client for the betuol server
-- ----------------------------

module Main where

import Network.Socket hiding           (recv)
import Network.Socket.ByteString       (recv)
import Data.ByteString.Char8 as B      (unpack)
import Control.Monad.Trans.State       (state, execState, runState)
import qualified Numeric.Matrix as Mat (unit, at, times, fromList)
import qualified Data.Map as Map       (lookup)

import Instance
import Event
import Math
import Component.Manager.Transform
import Component.Manager.Character
import Script
import Script.Scene1

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
            then error $ "wrong event recieved: should be approveCharacterCreationRequest; was " ++ typ
            else do
                -- get player's goid from the server make an Instance with that
                let (ApproveCharacterCreationRequestEvent id)  = getEvent evt' :: ApproveCharacterCreationRequestEvent
                let (eventType, is) = flip runState emptyInstanceState $ do
                    start id
                    update
                    reactEvent evt'
                -- putStrLn eventType

                loop sock is $ (run Scene1) ++ repeat (return . state $ \s -> ((), s))
                return ()

loop :: Socket -> InstanceState -> [IO (Instance ())] -> IO (Instance ())
loop sock is [] = return . state $ \s -> ((), s)
loop sock is (s:sx) = do
    -- run one scene of our scene script
    scene <- s
    let scene' = execState scene is

    -- then parse commands and junk like normal
    com <- getLine
    let (ret, is'@(InstanceState pl (TransformManager mats _) _ _)) = runState (parseInput com) scene'
    case ret of 
        (Right "quit") -> return . state $ const ((), is)
        otherwise -> do
            case ret of
                (Left err)      -> print err
                (Right "show")  -> print is'
                (Right "m")     -> let (Just (TransformComponent objType mat)) = Map.lookup pl mats
                                   in sendEvent sock $ CharacterMovedEvent pl
                                       [mat `Mat.at` (1,4), mat `Mat.at` (2,4), mat `Mat.at` (3,4)]
                otherwise       -> return ()

            loop sock (execState update is') sx

parseInput :: String -> Instance (Either String String)
parseInput line = do
    let args = words line
    state $ \s@(InstanceState pl tm@(TransformManager mats _) cm@(CharacterManager ids) _) ->
        if null args
        then (Left "no text printed", s)
        else let com = head args
            in case com of
            -- creates an object on the client
            -- takes 1 argument of ID to give object
            "create"  -> if length args < 2
                         then (Left "not enough arguments for `create` command", s)
                         else let n = read $ args !! 1
                              in (Right com, execState (createObjectSpecificID n $ buildTransformComponentJSON Open (Mat.unit 4)) s)
            -- movement command
            -- takes 1 argument of direction to move
            "m"       -> if length args < 2
                         then (Left "not enough arguments for `m` command", s)
                         else let (Just (TransformComponent objType mat)) = Map.lookup pl mats
                                  direction = case args !! 1 of
                                      "n" -> [ 0, 0, 1]
                                      "s" -> [ 0, 0,-1]
                                      "e" -> [ 1, 0, 0]
                                      "w" -> [-1, 0, 0]
                                      otherwise -> []
                              in if null direction
                                 then (Left $ "`" ++ args !! 1 ++ "` is not a valid direction.", s)
                                 else (Right com, execState (moveObject pl (mat `Mat.times` buildTranslationMatrix (4,4) direction)) s)
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

reactEvent :: EventDescriptor -> Instance String
reactEvent evt@(EventDescriptor typ evtData) =
    state $ \s@(InstanceState pl tm@(TransformManager mats _) cm@(CharacterManager ids) _) ->
    case typ of
        "attack" ->
            let ae@(AttackEvent (id1,id2)) = getEvent evt
            in (show ae, execState (attackObject id1 id2) s)
        "characterMoved" ->
            let ce@(CharacterMovedEvent id loc) = getEvent evt
                mat = Map.lookup id mats
                mat' = case mat of
                    (Just (TransformComponent objType mat'')) -> mat''
                    -- if we don't already have any information for this object, then make a new one and update it
                    -- will need to poll the server for data on this object since we don't have it yet
                    -- type information, etc.
                    Nothing -> let newId = execState (createObjectSpecificID id $ buildTransformComponentJSON Open (Mat.unit 4)) s
                                   (Just (TransformComponent objType mat'')) = Map.lookup id mats
                               in mat''
            in (show ce, execState (moveObject id (mat' `Mat.times` Mat.fromList [loc])) s)
        "approveCharacterCreationRequest" ->
            let accre@(ApproveCharacterCreationRequestEvent id) = getEvent evt
            in (show accre, execState (createObjectSpecificID id $ buildTransformComponentJSON Open (Mat.unit 4)) s)
        otherwise -> error $ "unsupported event type: " ++ typ ++ "\n\t data: " ++ show evtData
