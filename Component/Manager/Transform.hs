module Component.Manager.Transform
( moveComponent
) where

import Control.Monad.Trans.State (state)
import qualified Data.Map as Map
import qualified Numeric.Matrix as Mat
import Text.JSON
import Control.Monad
import Data.List
import Data.Maybe

import Component
import Common
import Math

instance JSON TransformComponent where
    showJSON (TransformComponent objType mat) = showJSON $ makeObj [
          ("ObjType", showJSON $ show objType)
        , ("Mat", showJSON (unwords. lines $ show mat))
        ]
    readJSON (JSObject obj) = do
        objType <- obj ! "ObjType" :: Result String
        mat     <- obj ! "Mat"     :: Result String
        let mats  = map read $ words mat
            -- split the joined list into 4 rows for the matrix
        let (mats1, matsx1) = splitAt 4 mats
        let (mats2, matsx2) = splitAt 4 matsx1
        let (mats3, matsx3) = splitAt 4 matsx2
        let mats' = [mats1, mats2, mats3, matsx3]
        return $ TransformComponent (read objType) (Mat.fromList mats')
    readJSON _ = mzero

instance ComponentCreator TransformManager where
    createComponent id objData (TransformManager mats grid) =
        let tc = readJSON objData :: Result TransformComponent
        in case tc of
            (Ok tc'@(TransformComponent _ mat)) -> let loc     = getGridXY mat
                                                       newGrid = updateGrid id loc Insert grid
                                                   in Right $ TransformManager (Map.insert id tc' mats) newGrid
            (Error err) -> error $ "creating transform component " ++ err
    update _ = return Nothing

data UpdateType = Insert | Delete
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

getGridXY :: Mat.Matrix Float -> (Int, Int)
getGridXY m = let x = m `Mat.at` (1, 4)
                  y = m `Mat.at` (3, 4)
              in (truncate x, truncate y)

moveComponent :: TransformManager -> GOiD -> Mat.Matrix Float -> Either String TransformManager
moveComponent (TransformManager mats grid) goid newLoc =
    let obj = Map.lookup goid mats
    in case obj of
        (Just (TransformComponent typ _)) ->
            let tc  = Map.lookup goid mats
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
        otherwise -> Left $ "there is no object with GOiD, " ++ show goid ++ ", that is able to be moved"

checkCollision :: (Int, Int) -> ComponentMap -> Grid -> GOiD 
checkCollision loc mats grid =
    let ids = Map.lookup loc grid
    in case ids of
        (Just ids') -> foldr checkBlocked (-1) (Map.toList $ Map.filterWithKey (\x _ -> x `elem` ids') mats)
                           where checkBlocked :: (GOiD, TransformComponent) -> GOiD -> GOiD
                                 checkBlocked (goid, TransformComponent Blocked _) _ = goid
                                 checkBlocked (_,    TransformComponent Open _)  acc = acc
        otherwise -> -1
