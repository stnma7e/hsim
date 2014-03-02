module Component.Manager.Transform
( TransformManager(..)
, moveComponent
) where

import qualified Data.Map as Map
import qualified Numeric.Matrix as Mat

import Component

data TransformManager = TransformManager (Map.Map GOiD (Mat.Matrix Float))
                        deriving Show

instance ComponentCreator TransformManager where
	createComponent id (TransformManager mats) = Right . TransformManager $ Map.insert id (Mat.unit 4) mats
	update m = Right m

moveComponent :: TransformManager -> GOiD -> Mat.Matrix Float -> TransformManager
moveComponent (TransformManager mats) goid newLoc = TransformManager $ Map.update (\x -> Just newLoc) goid mats
