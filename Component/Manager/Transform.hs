module Component.Manager.Transform
( TransformManager(..)
, moveComponent
) where

import qualified Data.Map as Map
import qualified Numeric.Matrix as Mat

import Component

data TransformManager = TransformManager (Map.Map GOiD (ObjectType, (Mat.Matrix Float)))
                        deriving Show

instance ComponentCreator TransformManager where
	createComponent id (TransformManager mats) = Right . TransformManager $ Map.insert id (Open, (Mat.unit 4)) mats
	update = Right

data ObjectType = Blocked
                | Open
                  deriving Show

moveComponent :: TransformManager -> GOiD -> Mat.Matrix Float -> Either String TransformManager
moveComponent (TransformManager mats) goid newLoc =
    let obj = Map.lookup goid mats
    in case obj of
        (Just (Open, _))    -> Right . TransformManager $ Map.update (\x -> Just (Open, newLoc)) goid mats
        (Just (Blocked, _)) -> Left "the space that was attempted to move to was blocked"
        otherwise           -> Left $ "there is no object with GOiD, " ++ (show goid)
