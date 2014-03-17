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
, buildJSON
) where 

import Control.Monad.Trans.State       (State(..), state, get, put, execState)
import qualified Data.Map as Map       (empty)
import qualified Numeric.Matrix as Mat (Matrix, unit, times, toList)
import Text.JSON
import Control.Monad

import Common
import Component
import Component.Manager.Transform
import Component.Manager.Character

type Instance = State InstanceState
data InstanceState = InstanceState
    { getPlayer           :: GOiD
    , getTransformManager :: TransformManager
    , getCharacterManager :: CharacterManager
    , objCount            :: GOiD
    } deriving (Show)

emptyInstanceState :: InstanceState
emptyInstanceState = InstanceState (-1) (TransformManager Map.empty Map.empty) (CharacterManager Map.empty) 0

buildJSON :: String -> String -> String
buildJSON tm cm = "{"
    ++ "\"Transform\": " ++ tm ++ ","
    ++ "\"Character\": " ++ cm ++ ""
    ++ "}"

start :: GOiD -> Instance ()
start playerId = do
    let json = buildJSON (buildTransformComponentJSON Open (Mat.unit 4)) (buildCharacterComponentJSON 10 5 10 Betuol)
    createObjectSpecificID playerId json
    (InstanceState _ tm cm oc) <- get
    put $ InstanceState playerId tm cm oc

update :: Instance ()
update = state $ \(InstanceState pl tm cm oc) ->
    ((), InstanceState pl (updateManager tm) (updateManager cm) oc)

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

createObject :: String -> Instance GOiD
createObject objData = state $ \s -> 
    let id = oc + 1
        (InstanceState pl tm cm oc) = execState (createObjectSpecificID id objData) s
    in (id, InstanceState pl tm cm id)

createObjectSpecificID :: GOiD -> String -> Instance ()
createObjectSpecificID idToMake objData = state $ \(InstanceState pl tm cm oc) ->
        let jsonObj = decode objData :: Result GameObjectJSON
        in case jsonObj of
            (Ok (GameObjectJSON jsonTm jsonCm)) -> ((), InstanceState pl (createObjectForManager idToMake jsonTm tm) (createObjectForManager idToMake jsonCm cm) oc)
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
moveObject id newLoc = state $ \s@(InstanceState pl tm cm oc) -> 
    let newTm = moveComponent tm id newLoc
    in case newTm of
        (Right tm') -> ("", InstanceState pl tm' cm oc)
        (Left err)  -> (err, s)

attackObject :: GOiD -> GOiD -> Instance ()
attackObject id1 id2 = state $ \(InstanceState pl tm cm oc) -> ((), InstanceState pl tm (attackComponent cm id1 id2) oc)
