module Component.Manager.Transform
( moveObject
, getExits
, getObjectLoc
) where

import Control.Monad.Trans.State (state, get, put)
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
    update _ = do
        evts <- getEventsFromInstance ["characterMoved"]
        updateFromEvents evts
        return Nothing

updateFromEvents :: [Event] -> Instance (Maybe String)
updateFromEvents [] = return Nothing
updateFromEvents (evt:evts) = do
    case evt of
        (CharacterMovedEvent id loc) -> do
            s <- get
            let (TransformManager mats _) = getTransformManager s
            let mat = Map.lookup id mats
            mat' <- case mat of
                    (Just (TransformComponent objType mat'')) -> return mat''
                    -- if we don't already have any information for this object, then make a new one and update it
                    -- will need to poll the server for data on this object since we don't have it yet
                    -- type information, etc.
                    Nothing -> return $ Mat.unit 4
            moveObject id (mat' `Mat.times` Mat.fromList [loc])
        otherwise -> return Nothing
    updateFromEvents evts 

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

moveObject :: GOiD -> Mat.Matrix Float -> Instance (Maybe String)
moveObject  goid newLoc = do
    s@(InstanceState _ tm@(TransformManager mats grid) _ _ _ _) <- get
    let obj = Map.lookup goid mats
    case obj of
        (Just (TransformComponent typ _)) ->
            let tc  = Map.lookup goid mats
                loc = getGridXY newLoc
            in case tc of
                (Just (TransformComponent typ mat)) ->
                    let collisionIds = checkCollision loc tm
                    in if not $ null collisionIds 
                       then return . Just $ "location " ++ show loc ++ " is blocked for GOiD " ++ show goid ++ ", by GOiD's " ++ show collisionIds ++ "."
                       else let oldLoc = getGridXY mat
                                gridWithOldDeleted = updateGrid goid oldLoc Delete grid
                                grid'  = updateGrid goid loc Insert gridWithOldDeleted
                            in do
                                put $ s { getTransformManager = TransformManager (Map.update (\_ -> Just $ TransformComponent typ newLoc) goid mats) grid' }
                                return Nothing
                otherwise -> return . Just $ "no matrix for component when moving; GOiD: " ++ show goid
        otherwise -> return . Just $ "there is no object with GOiD, " ++ show goid ++ ", that is able to be moved"

checkCollision :: (Int, Int) -> TransformManager -> [GOiD] 
checkCollision loc (TransformManager mats grid) =
    let ids = Map.lookup loc grid
    in case ids of
        (Just ids') -> foldr checkBlocked [] (Map.toList $ Map.filterWithKey (\x _ -> x `elem` ids') mats)
                           where checkBlocked :: (GOiD, TransformComponent) -> [GOiD] -> [GOiD]
                                 checkBlocked (goid, TransformComponent Blocked _) acc = goid : acc
                                 checkBlocked (_,    TransformComponent Open _)  acc = acc
        otherwise -> []

getObjectLoc :: GOiD -> TransformManager ->  (Int, Int)
getObjectLoc id (TransformManager mats _) = let (Just comp) = Map.lookup id mats
                                      in getGridXY (getMatrix comp)

data Direction = North | South | East | West
               | NWest | SWest
               | NEast | SEast
                 deriving (Show, Read)

getExits :: (Int, Int) -> TransformManager -> [Direction]
getExits (x,y) tm =
    let n = if null $ checkCollision (x + 1, y) tm
            then [North]
            else []
        s = if null $ checkCollision (x - 1, y) tm
            then [South]
            else []
        e = if null $ checkCollision (x, y + 1) tm
            then [East]
            else []
        w = if null $ checkCollision (x, y - 1) tm
            then [West]
            else []
    in n++s++e++w
