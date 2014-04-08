module Component.Manager.Transform
( TransformManager
, TransformComponent
, ObjectType
) where

import qualified Data.Map as Map
import qualified Numeric.Matrix as Mat

import Common

data ObjectType = Blocked | Open
 
data TransformComponent = TransformComponent
    { objType   :: ObjectType
    , getMatrix :: Mat.Matrix Float
    }

type ComponentMap = Map.Map GOiD TransformComponent

type Grid = Map.Map (Int, Int) [GOiD]

data TransformManager = TransformManager
    { matrices :: ComponentMap
    , grid     :: Grid
    }
