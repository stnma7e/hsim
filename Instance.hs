module Instance
( Component.Instance(..)
, Component.InstanceState(..)
, emptyInstanceState
, start
, Instance.update
, createObject
, createObjectSpecificID
, pushEvent
) where 

import Control.Monad.Trans.State       (state, get, put, execState)
import qualified Data.Map as Map
import qualified Numeric.Matrix as Mat
import Text.JSON
import Control.Monad
import Data.List
import Data.Maybe
import System.Random

import Event
import Common
import Component
import Component.Manager.Transform
import Component.Manager.Character
import Component.Manager.Ai

emptyInstanceState :: InstanceState
emptyInstanceState = InstanceState (-1) (TransformManager Map.empty Map.empty) (CharacterManager Map.empty) (AiManager Map.empty) Map.empty [0..100] (mkStdGen 0)

start :: StdGen -> Instance GOiD
start gen = do
    s <- get
    put $ s { randomNumGen = gen }

    playerId <- createObject $ buildObjectJSON (TransformComponent Open (Mat.unit 4)) (CharacterComponent 10 5 10 Betuol [(Betuol, 0)]) Enemy
    s <- get
    put $ s { player = playerId }
    return playerId 

update :: Instance (Map.Map String [Event])
update = do
    (InstanceState _ tm _ _ _ _ _) <- get
    tmErr <- Component.update tm
    case tmErr of
        (Just err) -> error $ "error when updating transform manager: " ++ err
        otherwise  -> return ()

    (InstanceState _ _ cm _ _ _ _) <- get
    cmErr <- Component.update cm
    case cmErr of
        (Just err) -> error $ "error when updating character manager: " ++ err
        otherwise  -> return ()

    (InstanceState _ _ _ am _ _ _) <- get
    amErr <- Component.update am
    case amErr of
        (Just err) -> error $ "error when updating ai manager: " ++ err
        otherwise  -> return ()

    s <- get
    let evts = getEvents s
    put $ s { getEvents = Map.empty }
    return evts

createObject :: JSValue -> Instance GOiD
createObject objData = state $ \s -> 
    let id = head (availiableIDS s)
        s' = execState (createObjectSpecificID id objData) s
    in (id, s' { availiableIDS = delete id (availiableIDS s') } )

createObjectSpecificID :: GOiD -> JSValue -> Instance ()
createObjectSpecificID idToMake objData = state $ \s@(InstanceState _ tm cm am _ _ _) ->
        let jsonObj = readJSON objData :: Result GameObjectJSON
        in case jsonObj of
            (Ok (GameObjectJSON jsonTm jsonCm jsonAm)) -> ((), s { transformManager = createObjectForManager idToMake jsonTm tm
                                                                 , characterManager = createObjectForManager idToMake jsonCm cm
                                                                 , aiManager        = createObjectForManager idToMake jsonAm am
                                                                 })
            (Error err) -> error err

createObjectForManager :: ComponentCreator a => GOiD -> JSValue -> a -> a
createObjectForManager idToMake objData cc =
    case createComponent idToMake objData cc of
        (Right cc') -> cc'
        (Left err)  -> error err
