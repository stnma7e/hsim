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
import Component hiding (update)
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
loop is [] = return ()
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
    if null args
    then return $ Right ""
    else let com = head args
        in case com of
        -- creates an object on the client
        -- takes 1 argument of ID to give object
        "create"  -> if length args < 2
                     then return $ Left "not enough arguments for `create` command"
                     else do
                         let n    = read $ args !! 1
                         let json = buildObjectJSON (TransformComponent Open (Mat.unit 4 )) (CharacterComponent 10 5 10 Betuol [(Betuol, 0)])
                         createObjectSpecificID n json
                         return $ Right com
        -- movement command
        -- takes 1 argument of direction to move
        "m"       -> do
            s <- get
            if length args < 2
            then return $ Left "not enough arguments for `m` command"
            else let (TransformManager mats _) = getTransformManager s
                     (Just (TransformComponent objType mat)) = Map.lookup (getPlayer s) mats
                     direction = case args !! 1 of
                         "n" -> [ 0, 0, 1]
                         "s" -> [ 0, 0,-1]
                         "e" -> [ 1, 0, 0]
                         "w" -> [-1, 0, 0]
                         otherwise -> []
                in if null direction
                   then return . Left $ "`" ++ args !! 1 ++ "` is not a valid direction."
                   else do
                       s <- get
                       err <- moveObject (getPlayer s) (mat `Mat.times` buildTranslationMatrix (4,4) direction)
                       if not $ null err
                       then return $ Left err
                       else return $ Right com
        -- attack command
        -- takes 1 argument of ID for player to attack
        "a"       -> if length args < 3
                     then return $ Left "not enough arguments for `a` command"
                     else do
                         s <- get
                         let n = read $ args !! 1
                         let loc = read $ args !! 2
                         attackObject (getPlayer s) n loc
                         return $ Right com
        -- for commands that need to use IO
        -- this block will just let specified commands fall through
        -- to be evaluated in the calling function with access to IO
        otherwise -> do
            s <- get
            if head args `elem` commands
            then return $ Right com
            else return $ Left "not a command"
                where commands :: [String]
                      commands = ["quit", "show", ""]

reactEvent :: EventDescriptor -> Instance String
reactEvent evt@(EventDescriptor typ evtData) =
    case getEvent evt of
        ae@(AttackEvent (id1, id2)) -> putEvent ae
        ce@(CharacterMovedEvent id loc) -> do
            s <- get
            let (TransformManager mats _) = getTransformManager s
            let mat = Map.lookup id mats
            let mat' = case mat of
                    (Just (TransformComponent objType mat'')) -> mat''
                    -- if we don't already have any information for this object, then make a new one and update it
                    -- will need to poll the server for data on this object since we don't have it yet
                    -- type information, etc.
                    Nothing -> let json = buildObjectJSON (TransformComponent Open (Mat.unit 4)) (CharacterComponent 10 5 10 Betuol [(Betuol, 0)])
                                   newId = execState (createObjectSpecificID id json) s
                                   (TransformManager mats _) = getTransformManager s
                                   (Just (TransformComponent objType mat'')) = Map.lookup id mats
                               in mat''
            err <- moveObject id (mat' `Mat.times` Mat.fromList [loc])
            return $ if null err
                     then show ce
                     else err
        otherwise -> error $ "unsupported event type: " ++ typ ++ "\n\t data: " ++ show evtData
