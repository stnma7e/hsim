module Instance
( Instance(..)
, InstanceState(..)
, emptyInstanceState
, start
, Instance.update
, createObject
, moveObject
) where 


import Control.Monad.Trans.State
import qualified Data.Map as Map
import qualified Numeric.Matrix as Mat

import Component
import Component.Manager.Transform
import Component.Manager.Character

type Instance         = State InstanceState
data InstanceState = InstanceState
    { getPlayer           :: GOiD
    , getTransformManager :: TransformManager
    , getCharacterManager :: CharacterManager
    } deriving (Show)

emptyInstanceState :: InstanceState
emptyInstanceState = InstanceState (-1) (TransformManager Map.empty) (CharacterManager Map.empty)

start :: Instance ()
start = do
    createObject 0
    (InstanceState _ tm cm) <- get
    put $ InstanceState 0 tm cm

update :: Instance ()
update = do
    updateTransformManager
    updateCharacterManager

updateTransformManager :: Instance ()
updateTransformManager = state $ \(InstanceState pl tm cm) ->
    let itm = case Component.update tm of
                  (Right tm') -> tm'
                  (Left err)  -> error err
    in ((), InstanceState pl itm cm)
updateCharacterManager :: Instance ()
updateCharacterManager = state $ \(InstanceState pl tm cm) ->
    let icm = case Component.update cm of
                  (Right cm') -> cm'
                  (Left err)  -> error err
    in ((), InstanceState pl tm icm)

addTransformManager :: TransformManager -> Instance ()
addTransformManager tm = state $ \(InstanceState pl _ cm) -> ((), InstanceState pl tm cm)
addCharacterManager :: CharacterManager -> Instance ()
addCharacterManager cm = state $ \(InstanceState pl tm _) -> ((), InstanceState pl tm cm)

createObject :: GOiD -> Instance ()
createObject idToMake = state $ \(InstanceState pl tm cm) -> 
    let itm = case createComponent idToMake tm of
                  (Right tm') -> tm'
                  (Left err)  -> error err
        icm = case createComponent idToMake cm of
                  (Right cm') -> cm'
                  (Left err) -> error err
    in ((), InstanceState pl itm icm)

moveObject :: GOiD -> Mat.Matrix Float ->  Instance ()
moveObject id newLoc = state $ \(InstanceState pl tm cm) -> ((), InstanceState pl (moveComponent tm id newLoc) cm)
