module Instance
( GOiD
, Instance(..)
, InstanceState(..)
, ComponentManager
, ComponentCreator
, start
, loop
, createObject
) where

import Control.Monad.Trans.State
import Text.Show.Functions
import qualified Data.Map as Map
import qualified Numeric.Matrix as Mat

type GOiD = Int
type AiFunction = GOiD -> ComponentManager -> ComponentManager -> State ComponentManager ()

type Instance         = State InstanceState
newtype InstanceState = InstanceState
    { getManagers :: [ComponentManager]
    } deriving (Show)

start :: Instance ()
start = do
    addManager (TransformManager Map.empty)
    addManager (CharManager Map.empty)
    createObject 0
	
loop :: GOiD -> Instance ()
loop 0 = state $ \s -> ((),s)
loop n = do
    s <- get
    put . InstanceState . map (\m -> let m' = update m
                                     in case m' of
                                         (Right m'') -> m''
                                         (Left err)  -> error err) $
        getManagers s
    loop (n-1)

createObject :: GOiD -> Instance ()
createObject idToMake = state $ \s -> ((), InstanceState $
    map (\m -> let m' = createComponent idToMake m
               in case m' of
                   (Right m'') -> m''
                   (Left err)  -> error err) $ 
        getManagers s
    )

addManager :: ComponentManager -> Instance ()
addManager m = state $ \s -> ((), InstanceState $
    m : getManagers s
    )

data Component = CharComponent [Int]
                 deriving (Show)
data ComponentManager = TransformManager (Map.Map GOiD (Mat.Matrix Float))
					  | CharManager (Map.Map GOiD (Component, AiFunction))
					    deriving (Show)

class ComponentCreator a where
	createComponent :: GOiD -> a -> Either String a
	update :: a -> Either String a
	
instance ComponentCreator ComponentManager where
	createComponent id (CharManager ids) = Right . CharManager $ Map.insert id (CharComponent [1,12,4,43,34,3], \gid m1 m2 -> state $ \s -> ((), s)) ids
	createComponent id (TransformManager mats) = Right (TransformManager (Map.insert id (Mat.matrix (4,4) (\(_,_) -> 0)) mats))
	
	update m = Right m
