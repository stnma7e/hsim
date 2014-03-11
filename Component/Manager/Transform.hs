module Component.Manager.Transform
( TransformManager(..)
, TransformComponent(..)
, ObjectType(..)
, moveComponent
, buildMatString
) where

import qualified Data.Map as Map
import qualified Numeric.Matrix as Mat
import Text.JSON
import Control.Monad
import Data.List

import Component
import Common
import Math

data TransformManager = TransformManager (Map.Map GOiD TransformComponent)
                        deriving Show

instance ComponentCreator TransformManager where
	createComponent id objData (TransformManager mats) = let tc = decode objData :: Result TransformComponent
                                                         in case tc of
            (Ok tc') -> Right . TransformManager $ Map.insert id tc' mats
            (Error err) -> error $ "here3 " ++ err
	update = Right

data ObjectType = Blocked
                | Open
                  deriving ( Show
                           , Read
                           )

data TransformComponent = TransformComponent ObjectType (Mat.Matrix Float)
                          deriving Show

instance JSON TransformComponent where
    showJSON = undefined
    readJSON (JSObject obj) = 
        let objType = obj ! "ObjType" :: Result String
            mat     = obj ! "Mat"     :: Result String
        in case objType of
            (Ok objType') -> return $ case mat of
                (Ok mat')   -> let mats  = map read $ words mat'
                                   -- split the joined list into 4 rows for the matrix
                                   (mats1, matsx1) = splitAt 4 mats
                                   (mats2, matsx2) = splitAt 4 matsx1
                                   (mats3, matsx3) = splitAt 4 matsx2
                                   mats' = [mats1, mats2, mats3, matsx3]
                               in TransformComponent (read objType') (Mat.fromList mats')
                (Error err) -> error $ "here " ++ err
            (Error err) -> error $ "here2 " ++ err
    readJSON _ = mzero

moveComponent :: TransformManager -> GOiD -> Mat.Matrix Float -> Either String TransformManager
moveComponent (TransformManager mats) goid newLoc =
    let obj = Map.lookup goid mats
    in case obj of
        (Just (TransformComponent typ _))    -> Right . TransformManager $ Map.update (\x -> Just $ TransformComponent typ newLoc) goid mats
        otherwise                             -> Left $ "there is no object with GOiD, , that is able to be moved" ++ show goid

buildMatString :: ObjectType -> Mat.Matrix Float -> String
buildMatString objType mat = "{"
                           ++ "\"ObjType\": \"" ++ show objType ++ "\",\n"
                           ++ "\"Mat\": \"" ++ (concat . intersperse " " . lines $ show mat) ++ "\"\n"
                           ++ "}"
