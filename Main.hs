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
import Component.Manager.Transform
import Component.Manager.Character
import Component.Manager.Ai

-- taken from Hackage
-- package: cgi
-- module: Network.CGI.Protocol
maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

startingInstance :: InstanceState
startingInstance = let objJSON = buildObjectJSON originLoc playerCharacter Guard
     in flip execState emptyInstanceState $ do
         playerId <- createObject objJSON
 
         s <- get
         put $ s { getInstancePlayer = playerId }
 
         _ <- update
         return playerId

  where originLoc       = TransformComponent Open (Mat.unit 4)
        playerCharacter = CharacterComponent 10 10 Betuol [(Betuol, 0)]
                          $ CharacterEquipment $ DamageType 5 [Melee, Frost]


main :: IO ()
main = do
    let objJSON = buildObjectJSON originLoc playerCharacter Guard
        is = flip execState emptyInstanceState $ do
            playerId <- createObject objJSON

            s <- get
            put $ s { getInstancePlayer = playerId }

            _ <- update
            return playerId
    
    _ <- loop is $ run Scene1 ++ repeat (return . state $ \s -> ((), s))
    return ()
  where originLoc       = TransformComponent Open (Mat.unit 4)
        playerCharacter = CharacterComponent 10 10 Betuol [(Betuol, 0)]
                          $ CharacterEquipment $ DamageType 5 [Melee, Frost]

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
        let (ret, is') = runState (handleInstanceResponse
                                      (head args)
                                      (tail args))
                                  scene'
        -- if the command entered needs to do some IO
        -- then it comes back here

        -- ret is going to be a Left  : error value
        --                    a Right : "quit"
        --                    a Right : regular command needing IO
        (com, reRunThisFrame) <- either reRunFrameBecauseThereWasAnError
                                        (\x -> let x' = words x
                                                in goToNextFrameNoError is'
                                                       (head x') (tail x'))
                                        ret

        putStrLn ""

        if com == "quit"
        then return . state $ const ((), is')
        else if reRunThisFrame
             then loop is' (return (return ()):sceneN)
             else do -- let's get a list of the events from the last frame
                     -- then we can use this information to display
                     -- character deaths, etc.
                     let (framesEvents, is'') = runState update is'
                     printEvents framesEvents

                     loop is'' sceneN
    where printEvents :: (EventList, EventList) -> IO ()
          printEvents (eventsFromLastFrame, eventsForNextFrame)= do
              putStrLn $ "events from last frame: " ++ show eventsFromLastFrame
              putStrLn $ "events for next frame: "  ++ show eventsForNextFrame
          -- the command was not valid and were not going to update the scene
          -- this is a Either Left value
          reRunFrameBecauseThereWasAnError :: String -> IO (String, Bool)
          reRunFrameBecauseThereWasAnError com =
              (const $ return (com, True)) =<< print com

          -- if the command is quit, then we do not want to re-run this frame
          -- _ we are dealing with a valid command,
          -- but, we still do not want to re-run the frame
          -- this is a Either Right value
          goToNextFrameNoError :: InstanceState
                               -> String
                               -> [String]
                               -> IO (String, Bool)
          goToNextFrameNoError is' com args =
              (const $ return (com, False)) =<< case com of
                  "show"  -> print is'
                  "look"  -> do
                      let tm = getManager Transform is'
                      putStrLn . (++) "Exits: " . show
                        . flip getExits tm
                        $ getObjectLoc (getInstancePlayer is') tm
                      -- print out the ids of the objects
                      -- in this space
                      print . filter (/= getInstancePlayer is') . map fst
                        . flip getObjectsAt tm
                        $ getObjectLoc (getInstancePlayer is') tm
                  "m"     -> let mat = getMatrix . fromJust . Map.lookup
                                       (getInstancePlayer is')
                                       $ getMatrices (getManager Transform is')
                             in print [ mat `Mat.at` (1,4)
                                      , mat `Mat.at` (2,4)
                                      , mat `Mat.at` (3,4)
                                      ]
                  "a"     -> if length args < 1
                             then error "no return from attackObject"
                             else print $ head args
                  _ -> return ()

handleInstanceResponse :: String -> [String] -> Instance (Either String String)
-- creates an object in the instance
-- takes 1 argument of ID to give object
handleInstanceResponse com@"create" args = 
    if length args < 2
    then return
       $ Left "not enough arguments for `create` command"
    else maybe (return $ Left "invalid ai type")
               correctAiType
               (maybeRead $ args !! 0)
  where validId :: GOiD -> AiComponent -> Instance (Either String String)
        validId idToMake computerType = do
            let json = buildObjectJSON (TransformComponent Open (Mat.unit 4 ))
                                       (CharacterComponent 10 10 Betuol
                                           [(Betuol, 0)] EmptyEquipment)
                                       computerType
            createObjectSpecificID idToMake json
            return $ Right com
        correctAiType :: GOiD -> Instance (Either String String)
        correctAiType idToMake = maybe (return $ Left "invalid id to create")
                                       (validId idToMake)
                                       (maybeRead $ args !! 1
                                           :: Maybe AiComponent)

-- movement command
-- takes 1 argument of direction to move
handleInstanceResponse com@"m" args =
    if length args < 1
    then return $ Left "not enough arguments for `m` command"
    else do s <- get
            if null directionToMove
            then return . Left
               $ "`" ++ args !! 0 ++ "` is not a valid direction." 
            else do err <- moveObject (player s) (newLoc s)
                    return $ maybe (Right com) Left err
  where player :: InstanceState -> GOiD
        player = getInstancePlayer
        mat :: InstanceState -> Mat.Matrix Float
        mat s = getMatrix . fromJust $ Map.lookup (player s)
                                                  (getMatrices $ getManager Transform s)
        directionToMove :: [Float]
        directionToMove = case args !! 0 of
            "n" -> [ 1,  0,  0]
            "s" -> [-1,  0,  0]
            "e" -> [ 0,  0,  1]
            "w" -> [ 0,  0, -1]
            _ -> []
        newLoc :: InstanceState -> Mat.Matrix Float
        newLoc s = mat s `Mat.times` buildTranslationMatrix (4,4)
                                         directionToMove

-- attack command
-- takes 1 argument of ID for player to attack
handleInstanceResponse com@"a" args =
    if length args < 2
    then return $ Left "not enough arguments for `a` command"
    else maybe (return $ Left "cannot read id to attack")
                attackWithReadId
                (maybeRead $ args !! 0)
  where hitTarget :: GOiD -> HitLocation -> Instance (Either String String)
        hitTarget idToAttack loc = do
            s <- get
            hitmiss <- attackObject (getInstancePlayer s) idToAttack loc
            return . Right $ com ++ " " ++ show hitmiss
        attackWithReadId :: GOiD -> Instance (Either String String)
        attackWithReadId idToAttack = maybe (return
                                           $ Left "cannot read hit location")
                                            (hitTarget idToAttack)
                                            (maybeRead $ args !! 1)

-- for commands that need to use IO
-- this block will just let specified commands fall through
-- to be evaluated in the calling function with access to IO
handleInstanceResponse com _ = return $ if com `elem` commands
                                        then Right com
                                        else Left "not a command"
    where commands = ["quit", "show", "look"]
