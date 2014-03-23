module Main where

import Control.Monad.Trans.State       (state, execState, runState, get)
import qualified Numeric.Matrix as Mat (unit, at, times, fromList)
import qualified Data.Map as Map       (lookup)
import System.Random
import Data.List
import Data.Maybe

import Instance
import Event
import Math
import Component hiding (update)
import Component.Manager.Transform
import Component.Manager.Character
import Script
import Script.Scene1

-- taken from Hackage
-- package: cgi
-- module: Network.CGI.Protocol
maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

main :: IO ()
main = do
    let gen = mkStdGen 1
    let (id, is) = flip runState emptyInstanceState $ do
        id <- start gen
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
                (Right "look")  -> let tm = getTransformManager is'
                                   in print $ getExits (getObjectLoc (getPlayer is') tm) tm
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
                     else case maybeRead $ args !! 1 of
                              (Just idToMake) -> do
                                  let json = buildObjectJSON (TransformComponent Open (Mat.unit 4 )) (CharacterComponent 10 5 10 Betuol [(Betuol, 0)])
                                  createObjectSpecificID idToMake json
                                  return $ Right com
                              otherwise -> return $ Left "invalid id to create"
        -- movement command
        -- takes 1 argument of direction to move
        "m"       -> do
            s <- get
            if length args < 2
            then return $ Left "not enough arguments for `m` command"
            else let (TransformManager mats _) = getTransformManager s
                     (Just (TransformComponent objType mat)) = Map.lookup (getPlayer s) mats
                     direction = case args !! 1 of
                         "n" -> [ 1,  0,  0]
                         "s" -> [-1,  0,  0]
                         "e" -> [ 0,  0,  1]
                         "w" -> [ 0,  0, -1]
                         otherwise -> []
                in if null direction
                   then return . Left $ "`" ++ args !! 1 ++ "` is not a valid direction."
                   else do
                       s <- get
                       err <- moveObject (getPlayer s) (mat `Mat.times` buildTranslationMatrix (4,4) direction)
                       return $ case err of
                           (Just err') -> Left err'
                           otherwise   -> Right com
        -- attack command
        -- takes 1 argument of ID for player to attack
        "a"       -> if length args < 3
                     then return $ Left "not enough arguments for `a` command"
                     else do
                         s <- get
                         case maybeRead $ args !! 1 of
                            (Just idToAttack) -> case maybeRead $ args !! 2 of
                                (Just loc) -> do
                                    attackObject (getPlayer s) idToAttack loc
                                    return $ Right com
                                otherwise -> return $ Left "cannot read hit location"
                            otherwise -> return $ Left "cannot read id to attack"
        -- for commands that need to use IO
        -- this block will just let specified commands fall through
        -- to be evaluated in the calling function with access to IO
        otherwise -> do
            s <- get
            return $ if head args `elem` commands
                     then Right com
                     else Left "not a command"
                where commands :: [String]
                      commands = ["quit", "show", "look", ""]
