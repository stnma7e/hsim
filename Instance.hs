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
import qualified Numeric.Matrix as Mat (Matrix)

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
    createObject
    (InstanceState _ tm cm oc) <- get
    put $ InstanceState playerId tm cm oc

update :: Instance ()
update = do
    updateTransformManager
    updateCharacterManager

updateTransformManager :: Instance ()
updateTransformManager = state $ \(InstanceState pl tm cm oc) ->
    let itm = case Component.update tm of
                  (Right tm') -> tm'
                  (Left err)  -> error err
    in ((), InstanceState pl itm cm oc)
updateCharacterManager :: Instance ()
updateCharacterManager = state $ \(InstanceState pl tm cm oc) ->
    let icm = case Component.update cm of
                  (Right cm') -> cm'
                  (Left err)  -> error err
    in ((), InstanceState pl tm icm oc)

createObject :: Instance GOiD
createObject = state $ \s -> 
    let (InstanceState pl tm cm oc) = execState (createObjectSpecificID oc) s
    in (oc, (InstanceState pl tm cm (oc + 1)))

createObjectSpecificID :: GOiD -> Instance ()
createObjectSpecificID idToMake = state $ \(InstanceState pl tm cm oc) -> 
    let itm = case createComponent idToMake tm of
                  (Right tm') -> tm'
                  (Left err)  -> error err
        icm = case createComponent idToMake cm of
                  (Right cm') -> cm'
                  (Left err) -> error err
    in ((), InstanceState pl itm icm oc)

moveObject :: GOiD -> Mat.Matrix Float ->  Instance ()
moveObject id newLoc = state $ \(InstanceState pl tm cm oc) -> ((), InstanceState pl (moveComponent tm id newLoc) cm oc)

attackObject :: GOiD -> GOiD -> Instance ()
attackObject id1 id2 = state $ \(InstanceState pl tm cm oc) -> ((), InstanceState pl tm (attackComponent cm id1 id2) oc)
