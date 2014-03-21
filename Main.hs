-- Client for the betuol server
-- ----------------------------

module Main where

import Control.Monad.Trans.State       (state, execState, runState, get)
import qualified Numeric.Matrix as Mat (unit, at, times, fromList)
import qualified Data.Map as Map       (lookup)
import Data.List

import Instance
import Event
import Math
import Component.Manager.Transform
import Component.Manager.Character
import Script
import Script.Scene1

main :: IO ()
main = do
    let (id, is) = flip runState emptyInstanceState $ do
        id <- start
        update
        return id
    
    loop is $ run Scene1 ++ repeat (return . state $ \s -> ((), s))
    return ()

loop :: InstanceState -> [IO (Instance ())] -> IO (Instance ())
loop is [] = return . state $ \s -> ((), s)
loop is (s:sx) = do
    -- run one scene of our scene script
    scene <- s
    let scene' = execState scene is

    -- then parse commands and junk like normal
    com <- getLine
    let (ret, is') = runState (parseInput com) scene'
    case ret of 
        (Right "quit") -> return . state $ const ((), is)
        otherwise -> do
            case ret of
                (Left err)      -> print err
                (Right "show")  -> print is'
                (Right "m")     -> let (TransformManager mats _) = getTransformManager is'
                                       (Just (TransformComponent objType mat)) = Map.lookup (getPlayer is') mats
                                   in print [mat `Mat.at` (1,4), mat `Mat.at` (2,4), mat `Mat.at` (3,4)]
                otherwise       -> return ()

            loop (execState update is') sx

parseInput :: String -> Instance (Either String String)
parseInput line = do
    let args = words line
    state $ \s ->
        if null args
        then (Right "", s)
        else let com = head args
            in case com of
            -- creates an object on the client
            -- takes 1 argument of ID to give object
            "create"  -> if length args < 2
                         then (Left "not enough arguments for `create` command", s)
                         else let n = read $ args !! 1
                                  json = buildObjectJSON (TransformComponent Open (Mat.unit 4 )) (CharacterComponent 10 5 10 Betuol)
                              in (Right com, execState (createObjectSpecificID n json) s)
            -- movement command
            -- takes 1 argument of direction to move
            "m"       -> if length args < 2
                         then (Left "not enough arguments for `m` command", s)
                         else let (TransformManager mats _) = getTransformManager s
                                  (Just (TransformComponent objType mat)) = Map.lookup (getPlayer s) mats
                                  direction = case args !! 1 of
                                      "n" -> [ 0, 0, 1]
                                      "s" -> [ 0, 0,-1]
                                      "e" -> [ 1, 0, 0]
                                      "w" -> [-1, 0, 0]
                                      otherwise -> []
                              in if null direction
                                 then (Left $ "`" ++ args !! 1 ++ "` is not a valid direction.", s)
                                 else let (err, s') = runState (moveObject (getPlayer s) (mat `Mat.times` buildTranslationMatrix (4,4) direction)) s
                                      in if not $ null err
                                         then (Left err, s)
                                         else (Right com, s')
            -- attack command
            -- takes 1 argument of ID for player to attack
            "a"       -> if length args < 2
                         then (Left "not enough arguments for `a` command", s)
                         else let n = read $ args !! 1
                                  is = execState (attackObject (getPlayer s) n) s
                              in (Right com, is)
            -- for commands that need to use IO
            -- this block will just let specified commands fall through
            -- to be evaluated in the calling function with access to IO
            otherwise -> if head args `elem` commands
                         then (Right com, s)
                         else (Left "not a command", s)
                             where commands :: [String]
                                   commands = ["quit", "show", ""]

reactEvent :: EventDescriptor -> Instance String
reactEvent evt@(EventDescriptor typ evtData) =
    state $ \s ->
    case typ of
        "attack" ->
            let ae@(AttackEvent (id1,id2)) = getEvent evt
            in (show ae, execState (attackObject id1 id2) s)
        "characterMoved" ->
            let ce@(CharacterMovedEvent id loc) = getEvent evt
                (TransformManager mats _) = getTransformManager s
                mat = Map.lookup id mats
                mat' = case mat of
                    (Just (TransformComponent objType mat'')) -> mat''
                    -- if we don't already have any information for this object, then make a new one and update it
                    -- will need to poll the server for data on this object since we don't have it yet
                    -- type information, etc.
                    Nothing -> let json = buildObjectJSON (TransformComponent Open (Mat.unit 4)) (CharacterComponent 10 5 10 Betuol)
                                   newId = execState (createObjectSpecificID id json) s
                                   (TransformManager mats _) = getTransformManager s
                                   (Just (TransformComponent objType mat'')) = Map.lookup id mats
                               in mat''
                (err, s') = runState (moveObject id (mat' `Mat.times` Mat.fromList [loc])) s
            in if not $ null err
               then (err, s)
               else (show ce, s')
        otherwise -> error $ "unsupported event type: " ++ typ ++ "\n\t data: " ++ show evtData
