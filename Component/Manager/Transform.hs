module Component.Manager.Transform
( TransformManager(..)
) where

import qualified Data.Map as Map
import qualified Numeric.Matrix as Mat

import Component

data TransformManager = TransformManager (Map.Map GOiD (Mat.Matrix Float))
                        deriving Show

instance ComponentCreator TransformManager where
	createComponent id (TransformManager mats) = Right . TransformManager $ Map.insert id (Mat.matrix (4,4) (\(_,_) -> 0)) mats
	update m = Right m
