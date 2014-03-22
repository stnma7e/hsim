module Instance
( Instance(..)
, InstanceState(..)
, emptyInstanceState
, start
, Instance.update
, createObject
, createObjectSpecificID

, moveObject
, attackObject
, buildObjectJSON
) where 

import Control.Monad.Trans.State       (State(..), state, get, put, execState)
import qualified Data.Map as Map       (Map(..), empty, insert, lookup)
import qualified Numeric.Matrix as Mat (Matrix, unit, times, toList)
import Text.JSON
import Control.Monad
import Data.List
import Data.Maybe

import Event
import Common
import Component
import Component.Manager.Transform
import Component.Manager.Character

type Instance = State InstanceState
data InstanceState = InstanceState
    { getPlayer           :: GOiD
    , getTransformManager :: TransformManager
    , getCharacterManager :: CharacterManager
    , getEvents           :: Map.Map String [Event]
    , availiableIDS       :: [GOiD]
    } deriving (Show)

emptyInstanceState :: InstanceState
emptyInstanceState = InstanceState (-1) (TransformManager Map.empty Map.empty) (CharacterManager Map.empty) Map.empty [0..100]

buildObjectJSON :: (JSON a, JSON b) => a -> b -> JSValue
buildObjectJSON tm cm = showJSON $ makeObj [("Transform", showJSON tm), ("Character", showJSON cm)]

start :: Instance GOiD
start = do
    playerId <- createObject $ buildObjectJSON (TransformComponent Open (Mat.unit 4)) (CharacterComponent 10 5 10 Betuol)
    s <- get
    put $ s { getPlayer = playerId }
    return playerId 

update :: Instance ()
update = do
    (InstanceState _ tm _ _ _) <- get
    let tm' = case Component.update tm of
            (Left err) -> error err
            (Right tm') -> tm'
    (InstanceState _ _ cm _ _) <- get
    let cm' = case Component.update cm of
            (Left err) -> error err
            (Right cm') -> cm'

    s <- get
    put $ s { getTransformManager = tm'
            , getCharacterManager = cm'
            }

createObject :: JSValue -> Instance GOiD
createObject objData = state $ \s -> 
    let id = head (availiableIDS s)
        s' = execState (createObjectSpecificID id objData) s
    in (id, s' { availiableIDS = delete id (availiableIDS s') } )

createObjectSpecificID :: GOiD -> JSValue -> Instance ()
createObjectSpecificID idToMake objData = state $ \s@(InstanceState _ tm cm _ _) ->
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

pushEvent :: Event -> Instance ()
pushEvent evt = insertEvent evt $ case evt of
        (AttackEvent _) -> eventTypeAttack
    where insertEvent :: Event -> String -> Instance ()
          insertEvent evt typ = state $ \s ->
              let evts = getEvents s
                  eventsOfCurrentType = Map.lookup typ evts
                  newEventList = case eventsOfCurrentType of
                      (Just curEvts) -> Map.insert typ (evt : curEvts) evts
                      otherwise      -> Map.insert typ [evt] evts
                  in ((), s { getEvents = newEventList})

--
-- Wrapper functions
--

moveObject :: GOiD -> Mat.Matrix Float -> Instance String
moveObject id newLoc = state $ \s ->
    let newTm = moveComponent (getTransformManager s) id newLoc
    in case newTm of
        (Right tm') -> ("", s { getTransformManager = tm' })
        (Left err)  -> (err, s)

attackObject :: GOiD -> GOiD -> Instance ()
attackObject id1 id2 = state $ \s -> ((), s { getCharacterManager = attackComponent (getCharacterManager s) id1 id2 })
