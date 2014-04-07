module Main where

import Control.Monad.Trans.State
import qualified Numeric.Matrix as Mat
import qualified Data.Map as Map
import Data.Maybe
import Control.Monad

import Instance
import Math
import Component hiding (update)
import Script
import Script.Scene1

-- taken from Hackage
-- package: cgi
-- module: Network.CGI.Protocol
maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

main :: IO ()
main = do
    let is = flip execState emptyInstanceState $ do
            let objJSON = buildObjectJSON (TransformComponent Open (Mat.unit 4)) 
                                          (CharacterComponent 10 10 Betuol [(Betuol, 0)] (CharacterEquipment $ DamageType 5 [Melee, Frost]))
                                          Guard
            playerId <- createObject objJSON

            s <- get
            put $ s { getInstancePlayer = playerId }

            _ <- update
            return playerId
    
    _ <- loop is $ run Scene1 ++ repeat (return . state $ \s -> ((), s))
    return ()

loop :: InstanceState -> [IO (Instance ())] -> IO (Instance ())
loop _ [] = return $ return ()
loop is (scene1:sceneN) = do
    -- run one scene of our scene script
    -- we have to lift execState into the IO monad so that it can accept
    -- the IO script function, and so that it can operate within the IO
    -- monad in loop.
    scene' <- liftM (flip execState is) scene1

    -- then parse commands and junk like normal
    args <- liftM words getLine
    if null args
    then loop scene' (return (return ()):sceneN)
    else do
        -- check which command has just been entered
        -- and react to the command by modifing the instance
        let (ret, is') = runState (handleInstanceResponse (head args) (tail args)) scene'
        -- if the command entered needs to do some IO
        -- then it comes back here

        -- ret is going to be a Left  : error value
        --                    a Right : "quit"
        --                    a Right : regular command needing IO
        (com, reRunThisFrame) <- either (reRunFrameBecauseThereWasAnError is')
                                        (\x -> let x' = words x
                                                in goToNextFrameNoError is' (head x') (tail x'))
                                        ret

        putStrLn ""

        if com == "quit"
        then return . state $ const ((), is')
        else if reRunThisFrame
             then loop is' (return (return ()):sceneN)
             else do
                 -- let's get a list of the events from the last frame
                 -- then we can use this information to display character deaths, etc.
                 let ((eventsFromLastFrame, eventsForNextFrame), is'') = runState update is'
                 putStrLn $ "events from last frame: " ++ show eventsFromLastFrame
                 putStrLn $ "events for next frame: "  ++ show eventsForNextFrame

                 loop is'' sceneN

    where -- the command was not valid and were not going to update the scene
          -- this is a Either Left value
          reRunFrameBecauseThereWasAnError :: InstanceState -> String -> IO (String, Bool)
          reRunFrameBecauseThereWasAnError _ com = (const $ return (com, True)) =<< print com

          -- if the command is quit, then we do not want to re-run this frame
          -- _ we are dealing with a valid command,
          -- but, we still do not want to re-run the frame
          -- this is a Either Right value
          goToNextFrameNoError :: InstanceState -> String -> [String] -> IO (String, Bool)
          goToNextFrameNoError is' com args = (const $ return (com, False)) =<< case com of
              "show"  -> print is'
              "look"  -> let tm = transformManager is'
                         in do
                             putStrLn . (++) "Exits: " . show $ getExits (getObjectLoc (getInstancePlayer is') tm) tm
                             -- print out the ids of the objects in this space
                             print . filter (/= getInstancePlayer is') . map fst . flip getObjectsAt (transformManager is') $
                                getObjectLoc (getInstancePlayer is') (transformManager is')
              "m"     -> let (TransformManager mats _) = transformManager is'
                             (Just (TransformComponent _ mat)) = Map.lookup (getInstancePlayer is') mats
                         in print [mat `Mat.at` (1,4), mat `Mat.at` (2,4), mat `Mat.at` (3,4)]
              "a"     -> if length args < 1
                         then error "no return from attackObject"
                         else print $ head args
              _ -> return ()

handleInstanceResponse :: String -> [String] -> Instance (Either String String)
handleInstanceResponse com args = case com of
    -- creates an object in the instance
    -- takes 1 argument of ID to give object
    "create"  -> if length args < 2
                 then return $ Left "not enough arguments for `create` command"
                 else case maybeRead $ args !! 0 of
                          (Just idToMake) -> case maybeRead $ args !! 1 :: Maybe AiComponent of 
                              (Just computerType) -> do
                                  let json = buildObjectJSON (TransformComponent Open (Mat.unit 4 ))
                                                             (CharacterComponent 10 10 Betuol [(Betuol, 0)] EmptyEquipment)
                                                             computerType
                                  createObjectSpecificID idToMake json
                                  return $ Right com
                              _ -> return $ Left "invalid ai type"
                          _ -> return $ Left "invalid id to create"
    -- movement command
    -- takes 1 argument of direction to move
    "m"       -> do
        s <- get
        if length args < 1
        then return $ Left "not enough arguments for `m` command"
        else let (TransformManager mats _) = transformManager s
                 (Just (TransformComponent _ mat)) = Map.lookup (getInstancePlayer s) mats
                 direction = case args !! 0 of
                     "n" -> [ 1,  0,  0]
                     "s" -> [-1,  0,  0]
                     "e" -> [ 0,  0,  1]
                     "w" -> [ 0,  0, -1]
                     _ -> []
            in if null direction
               then return . Left $ "`" ++ args !! 0 ++ "` is not a valid direction."
               else do
                   s' <- get
                   err <- moveObject (getInstancePlayer s') (mat `Mat.times` buildTranslationMatrix (4,4) direction)
                   return $ case err of
                       (Just err') -> Left err'
                       _   -> Right com
    -- attack command
    -- takes 1 argument of ID for player to attack
    "a"       -> if length args < 2
                 then return $ Left "not enough arguments for `a` command"
                 else do
                     s <- get
                     case maybeRead $ args !! 0 of
                        (Just idToAttack) -> case maybeRead $ args !! 1 of
                            (Just loc) -> do
                                hitmiss <- attackObject (getInstancePlayer s) idToAttack loc
                                return . Right $ com ++ " " ++ show hitmiss
                            _ -> return $ Left "cannot read hit location"
                        _ -> return $ Left "cannot read id to attack"
    -- for commands that need to use IO
    -- this block will just let specified commands fall through
    -- to be evaluated in the calling function with access to IO
    _ -> return $ if com `elem` commands
                          then Right com
                          else Left "not a command"
            where commands = ["quit", "show", "look"]
