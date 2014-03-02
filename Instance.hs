module Instance
( Instance(..)
, InstanceState(..)
, emptyInstanceState
, start
, Instance.update
, createObject) where 

import Control.Monad.Trans.State
import qualified Data.Map as Map
import qualified Numeric.Matrix as Mat

import Component
import Component.Manager.Transform
import Component.Manager.Character

type Instance         = State InstanceState
data InstanceState = InstanceState
    { getTransformManager :: TransformManager
    , getCharacterManager :: CharacterManager
    } deriving (Show)

emptyInstanceState :: InstanceState
emptyInstanceState = InstanceState (TransformManager Map.empty) (CharacterManager Map.empty)

start :: Instance ()
start = createObject 0

update :: Instance ()
update = do
    updateTransformManager
    updateCharacterManager

updateTransformManager :: Instance ()
updateTransformManager = state $ \(InstanceState tm cm) ->
    let itm = case Component.update tm of
                  (Right tm') -> tm'
                  (Left err)  -> error err
    in ((), InstanceState itm cm)
updateCharacterManager :: Instance ()
updateCharacterManager = state $ \(InstanceState tm cm) ->
    let icm = case Component.update cm of
                  (Right cm') -> cm'
                  (Left err)  -> error err
    in ((), InstanceState tm icm)

addTransformManager :: TransformManager -> Instance ()
addTransformManager tm = state $ \(InstanceState _ cm) -> ((), InstanceState tm cm)
addCharacterManager :: CharacterManager -> Instance ()
addCharacterManager cm = state $ \(InstanceState tm _) -> ((), InstanceState tm cm)

createObject :: GOiD -> Instance ()
createObject idToMake = state $ \(InstanceState tm cm) -> 
    let itm = case createComponent idToMake tm of
                  (Right tm') -> tm'
                  (Left err)  -> error err
        icm = case createComponent idToMake cm of
                  (Right cm') -> cm'
                  (Left err) -> error err
    in ((), InstanceState itm icm)

moveCharacter :: GOiD -> Mat.Matrix Float ->  Instance ()
moveCharacter id newLoc = state $ \s -> ((), emptyInstanceState)
