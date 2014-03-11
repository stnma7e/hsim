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
) where 

import Control.Monad.Trans.State       (State(..), state, get, put, execState)
import qualified Data.Map as Map       (empty)
import qualified Numeric.Matrix as Mat (Matrix, unit, times, toList)

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
emptyInstanceState = InstanceState (-1) (TransformManager Map.empty) (CharacterManager Map.empty) 0

start :: GOiD -> Instance ()
start playerId = do
    createObjectSpecificID playerId $ buildMatString Open (Mat.unit 4)
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

createObject :: String -> Instance GOiD
createObject objData = state $ \s -> 
    let id = oc + 1
        (InstanceState pl tm cm oc) = execState (createObjectSpecificID id objData) s
    in (id, InstanceState pl tm cm id)

createObjectSpecificID :: GOiD -> String -> Instance ()
createObjectSpecificID idToMake objData = state $ \(InstanceState pl tm cm oc) ->
    ((), InstanceState pl (createObjectForManager idToMake objData tm) (createObjectForManager idToMake objData cm) oc)

createObjectForManager :: ComponentCreator a => GOiD -> String -> a -> a
createObjectForManager idToMake objData cc =
    case createComponent idToMake objData cc of
        (Right cc') -> cc'
        (Left err)  -> error err
    
--
-- Wrapper functions
--

moveObject :: GOiD -> Mat.Matrix Float -> Instance ()
moveObject id newLoc = state $ \s@(InstanceState pl tm cm oc) -> 
    let newTm = moveComponent tm id newLoc
    in case newTm of
        (Right tm') -> ((), InstanceState pl tm' cm oc)
        (Left err)  -> error err

attackObject :: GOiD -> GOiD -> Instance ()
attackObject id1 id2 = state $ \(InstanceState pl tm cm oc) -> ((), InstanceState pl tm (attackComponent cm id1 id2) oc)
