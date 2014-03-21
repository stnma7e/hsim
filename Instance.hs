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
import qualified Data.Map as Map       (empty)
import qualified Numeric.Matrix as Mat (Matrix, unit, times, toList)
import Text.JSON
import Control.Monad
import Data.List

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
    , getEventManager     :: [Event]
    , availiableIDS       :: [GOiD]
    } deriving (Show)

emptyInstanceState :: InstanceState
emptyInstanceState = InstanceState (-1) (TransformManager Map.empty Map.empty) (CharacterManager Map.empty) [] [0..100]

buildObjectJSON :: (JSON a, JSON b) => a -> b -> JSValue
buildObjectJSON tm cm = showJSON $ makeObj [("Transform", showJSON tm), ("Character", showJSON cm)]

start :: Instance (GOiD)
start = do
    let json = buildObjectJSON (TransformComponent Open (Mat.unit 4)) (CharacterComponent 10 5 10 Betuol)
    playerId <- createObject json
    (InstanceState _ tm cm evts oc) <- get
    put $ InstanceState playerId tm cm evts (delete playerId oc)
    return playerId

update :: Instance ()
update = state $ \(InstanceState pl tm cm evts oc) ->
    ((), InstanceState pl (updateManager tm) (updateManager cm) evts oc)

updateManager :: ComponentCreator a => a -> a
updateManager cc =
    case Component.update cc of
        (Right cc') -> cc'
        (Left err)  -> error err

data GameObjectJSON = GameObjectJSON
    { transform :: JSValue
    , character :: JSValue
    }

instance JSON GameObjectJSON where
    showJSON = undefined
    readJSON (JSObject obj) = 
        let tm = obj ! "Transform" :: Result JSValue
            cm = obj ! "Character" :: Result JSValue
        in case tm of
            (Ok tm') -> return $ case cm of
                (Ok cm')    -> GameObjectJSON tm' cm' 
                (Error err) -> error $ "unable to determine `Character` from JSON component " ++ err
            (Error err) -> error $ "unable to determine `Transform` from JSON component " ++ err
    readJSON _ = mzero

createObject :: JSValue -> Instance GOiD
createObject objData = state $ \s -> 
    let id = head oc
        (InstanceState pl tm cm evts oc) = execState (createObjectSpecificID id objData) s
    in (id, InstanceState pl tm cm evts (delete id oc))

createObjectSpecificID :: GOiD -> JSValue -> Instance ()
createObjectSpecificID idToMake objData = state $ \(InstanceState pl tm cm evts oc) ->
        let jsonObj = readJSON objData :: Result GameObjectJSON
        in case jsonObj of
            (Ok (GameObjectJSON jsonTm jsonCm)) -> ((), InstanceState pl (createObjectForManager idToMake jsonTm tm) (createObjectForManager idToMake jsonCm cm) evts oc)
            (Error err) -> error err

createObjectForManager :: ComponentCreator a => GOiD -> JSValue -> a -> a
createObjectForManager idToMake objData cc =
    case createComponent idToMake objData cc of
        (Right cc') -> cc'
        (Left err)  -> error err

--
-- Wrapper functions
--

moveObject :: GOiD -> Mat.Matrix Float -> Instance String
moveObject id newLoc = state $ \s@(InstanceState pl tm cm evts oc) -> 
    let newTm = moveComponent tm id newLoc
    in case newTm of
        (Right tm') -> ("", InstanceState pl tm' cm evts oc)
        (Left err)  -> (err, s)

attackObject :: GOiD -> GOiD -> Instance ()
attackObject id1 id2 = state $ \(InstanceState pl tm cm evts oc) -> ((), InstanceState pl tm (attackComponent cm id1 id2) evts oc)
