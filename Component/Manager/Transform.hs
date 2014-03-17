module Component.Manager.Transform
( TransformManager(..)
, TransformComponent(..)
, ObjectType(..)
, moveComponent
, buildTransformComponentJSON
) where

import qualified Data.Map as Map
import qualified Numeric.Matrix as Mat
import Text.JSON
import Control.Monad
import Data.List
import Data.Maybe

import Component
import Common
import Math

type ComponentMap = Map.Map GOiD TransformComponent
type Grid = Map.Map (Int, Int) [GOiD]

data TransformManager = TransformManager
    { components       :: ComponentMap
    , spatialPartition :: Grid
    }
                        deriving Show

instance ComponentCreator TransformManager where
    createComponent id objData (TransformManager mats grid) =
        let tc = readJSON objData :: Result TransformComponent
        in case tc of
            (Ok tc'@(TransformComponent _ mat)) -> let loc     = getGridXY mat
                                                       newGrid = updateGrid id loc Insert grid
                                                   in Right $ TransformManager (Map.insert id tc' mats) newGrid
            (Error err) -> error $ "creating transform component " ++ err
    update = Right

data UpdateType = Insert
                | Delete
                  deriving Show
updateGrid :: GOiD -> (Int, Int) -> UpdateType -> Grid -> Grid
updateGrid goid loc (Insert) grid = if Map.member loc grid
                                    then Map.update (Just . (++) [goid]) loc grid
                                    else Map.insert loc [goid] grid
updateGrid goid loc (Delete) grid = let newGrid = Map.update (Just . foldr (\x acc -> if goid == x then acc else x:acc) []) loc grid
                                        gridSpace = Map.lookup loc newGrid
                                    in case gridSpace of
                                        (Just []) -> Map.delete loc grid
                                        otherwise -> newGrid

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
                (Error err) -> error $ "unable to determine `Mat` from JSON component " ++ err
            (Error err) -> error $ "unable to determine `ObjType` from JSON component " ++ err
    readJSON _ = mzero

getGridXY :: Mat.Matrix Float -> (Int, Int)
getGridXY m = let x = m `Mat.at` (1, 4)
                  y = m `Mat.at` (3, 4)
              in (truncate x, truncate y)

moveComponent :: TransformManager -> GOiD -> Mat.Matrix Float -> Either String TransformManager
moveComponent (TransformManager mats grid) goid newLoc =
    let obj = Map.lookup goid mats
    in case obj of
        (Just (TransformComponent typ _))    -> let tc  = Map.lookup goid mats
                                                    loc = getGridXY newLoc
                                                in case tc of
                                                    (Just (TransformComponent typ mat)) ->
                                                        let collisionId = checkCollision loc mats grid
                                                        in if collisionId > 0
                                                           then Left $ "location " ++ show loc ++ " is blocked for GOiD " ++ show goid ++ ", by GOiD " ++ show collisionId ++ "."
                                                           else let oldLoc = getGridXY mat
                                                                    gridWithOldDeleted = updateGrid goid oldLoc Delete grid
                                                                    grid'  = updateGrid goid loc Insert gridWithOldDeleted
                                                                in Right $ TransformManager (Map.update (\_ -> Just $ TransformComponent typ newLoc) goid mats) grid'
                                                    otherwise -> Left $ "no matrix for component when moving; GOiD: " ++ show goid
        otherwise                            -> Left $ "there is no object with GOiD, " ++ show goid ++ ", that is able to be moved"

checkCollision :: (Int, Int) -> ComponentMap -> Grid -> GOiD 
checkCollision loc mats grid = let ids = Map.lookup loc grid
                               in case ids of
                                   (Just ids') -> foldr checkBlocked (-1) (Map.toList $ Map.filterWithKey (\x _ -> x `elem` ids') mats)
                                                      where checkBlocked :: (GOiD, TransformComponent) -> GOiD -> GOiD
                                                            checkBlocked (goid, TransformComponent Blocked _) _ = goid
                                                            checkBlocked (_,    TransformComponent Open _)  acc = acc
                                   otherwise -> -1

buildTransformComponentJSON :: ObjectType -> Mat.Matrix Float -> String
buildTransformComponentJSON objType mat = "{"
                           ++ "\"ObjType\": \"" ++ show objType ++ "\","
                           ++ "\"Mat\": \"" ++ (unwords . lines $ show mat) ++ "\""
                           ++ "}"
