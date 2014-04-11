module Instance
( Component.Instance
, Component.InstanceState
, emptyInstanceState
, Instance.update
, createObject
, createObjectSpecificID
, pushEvent
) where 

import Control.Monad.Trans.State
import qualified Data.Map as Map
import Text.JSON
import Data.List
import Control.Monad
import System.Random

import Event
import Component
import Component.Manager.Transform
import Component.Manager.Character
import Component.Manager.Ai

emptyInstanceState :: InstanceState
emptyInstanceState = InstanceState { getInstancePlayer = -1
                                   , getEvents         = (Map.empty, Map.empty)
                                   , availiableIDS     = [0..100]
                                   , randomNumGen      = mkStdGen 0
                                   , managers          = [ (Transform, ComponentManager $ TransformManager Map.empty Map.empty)
                                                         , (Character, ComponentManager $ CharacterManager Map.empty)
                                                         , (Ai, ComponentManager $ AiManager Map.empty)
                                                         ]
                                   }

update :: Instance (Map.Map String [Event], Map.Map String [Event])
update = do
    am <- liftM aiManager get
    amErr <- Component.update am
    case amErr of
        (Just err) -> error $ "error when updating ai manager: " ++ err
        _  -> return ()

    cm <- liftM characterManager get
    cmErr <- Component.update cm
    case cmErr of
        (Just err) -> error $ "error when updating character manager: " ++ err
        _  -> return ()

    tm <- liftM transformManager get
    tmErr <- Component.update tm
    case tmErr of
        (Just err) -> error $ "error when updating transform manager: " ++ err
        _  -> return ()

    s <- get
    let (currentFrameEvents, nextFrameEvents) = getEvents s
    put $ s { getEvents = (nextFrameEvents, Map.empty) }
    return (currentFrameEvents, nextFrameEvents)

createObject :: JSValue -> Instance GOiD
createObject objData = state $ \s ->
    let goid = head (availiableIDS s)
        s' = execState (createObjectSpecificID goid objData) s
    in (goid, s' { availiableIDS = delete goid (availiableIDS s') } )

createObjectSpecificID :: GOiD -> JSValue -> Instance ()
createObjectSpecificID idToMake objData = state $ \s ->
        let tm = transformManager s
            cm = characterManager s
            am = aiManager s
            jsonObj = readJSON objData :: Result GameObjectJSON
        in case jsonObj of
            (Ok (GameObjectJSON jsonTm jsonCm jsonAm)) -> let is1 = putManager Transform (ComponentManager $ createObjectForManager idToMake jsonTm tm) s
                                                              is2 = putManager Character (ComponentManager $ createObjectForManager idToMake jsonCm cm) is1
                                                              is3 = putManager Ai (ComponentManager $ createObjectForManager idToMake jsonAm am) is2
                                                          in ((), is3)
            (Error err) -> error err

createObjectForManager :: ComponentCreator a => GOiD -> JSValue -> a -> a
createObjectForManager idToMake objData cc =
    case createComponent idToMake objData cc of
        (Right cc') -> cc'
        (Left err)  -> error err
