module Instance
( Component.Instance(..)
, Component.InstanceState(..)
, emptyInstanceState
, start
, Instance.update
, createObject
, createObjectSpecificID
, pushEvent

, buildObjectJSON
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

emptyInstanceState :: InstanceState
emptyInstanceState = InstanceState (-1) (TransformManager Map.empty Map.empty) (CharacterManager Map.empty) Map.empty [0..100] (mkStdGen 0)

buildObjectJSON :: (JSON a, JSON b) => a -> b -> JSValue
buildObjectJSON tm cm = showJSON $ makeObj [("Transform", showJSON tm), ("Character", showJSON cm)]

start :: StdGen -> Instance GOiD
start gen = do
    s <- get
    put $ s { randomNumGen = gen }

    playerId <- createObject $ buildObjectJSON (TransformComponent Open (Mat.unit 4)) (CharacterComponent 10 5 10 Betuol [(Betuol, 0)])
    s <- get
    put $ s { getPlayer = playerId
            {-, getEvents = Map.fromList [ ("attack", [ AttackEvent (1,1)-}
                                                    {-, AttackEvent (2,2)-}
                                                    {-])-}
                                       {-, ("blah", [])]-}
            }
    return playerId 

update :: Instance ()
update = do
    (InstanceState _ tm _ _ _ _) <- get
    tmErr <- Component.update tm
    case tmErr of
        (Just err) -> error $ "error when updating transform manager: " ++ err
        otherwise  -> return ()

    (InstanceState _ _ cm _ _ _) <- get
    cmErr <- Component.update cm
    case cmErr of
        (Just err) -> error $ "error when updating character manager: " ++ err
        otherwise  -> return ()

createObject :: JSValue -> Instance GOiD
createObject objData = state $ \s -> 
    let id = head (availiableIDS s)
        s' = execState (createObjectSpecificID id objData) s
    in (id, s' { availiableIDS = delete id (availiableIDS s') } )

createObjectSpecificID :: GOiD -> JSValue -> Instance ()
createObjectSpecificID idToMake objData = state $ \s@(InstanceState _ tm cm _ _ _) ->
        let jsonObj = readJSON objData :: Result GameObjectJSON
        in case jsonObj of
            (Ok (GameObjectJSON jsonTm jsonCm)) -> ((), s { getTransformManager = createObjectForManager idToMake jsonTm tm
                                                          , getCharacterManager = createObjectForManager idToMake jsonCm cm
                                                          })
            (Error err) -> error err

createObjectForManager :: ComponentCreator a => GOiD -> JSValue -> a -> a
createObjectForManager idToMake objData cc =
    case createComponent idToMake objData cc of
        (Right cc') -> cc'
        (Left err)  -> error err

pushEvent :: Event -> Instance String
pushEvent evt = insertEvent evt $ case evt of
        (AttackEvent _) -> eventTypeAttack
    where insertEvent :: Event -> String -> Instance String
          insertEvent evt typ = state $ \s ->
              let evts = getEvents s
                  eventsOfCurrentType = Map.lookup typ evts
                  newEventList = case eventsOfCurrentType of
                      (Just curEvts) -> Map.insert typ (evt : curEvts) evts
                      otherwise      -> Map.insert typ [evt] evts
                  in (show evt, s { getEvents = newEventList})
