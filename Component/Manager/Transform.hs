{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

module Component.Manager.Transform
( TransformManager(..)
, TransformComponent(..)

, moveObject
, ObjectOccupancy(..)

, getObjectLoc
, getObjectMatrix
, getObjectsAt

, getExits

-- Testing
, getGridXY
, checkCollision
) where

import Control.Monad.Trans.State
import Text.JSON
import Control.Monad
import Control.Applicative
import qualified Data.Map as Map
import qualified Data.Packed.Matrix as Mat
import qualified Numeric.Container as Mat

import Component
import Common

data ObjectOccupancy = Blocked | Open
                deriving (Show , Read, Eq)

data TransformComponent = TransformComponent
    { getObjType :: ObjectOccupancy
    , getMatrix  :: Mat.Matrix Float
    }
deriving instance Eq (Mat.Matrix Float) => Eq TransformComponent
deriving instance Show (Mat.Matrix Float) => Show TransformComponent

type ComponentMap = Map.Map GOiD TransformComponent

type Grid = Map.Map (Int, Int) [GOiD]

data TransformManager = TransformManager
    { getMatrices :: ComponentMap
    , getGrid     :: Grid
    } deriving Show

instance JSON TransformComponent where
    showJSON (TransformComponent objType mat) = showJSON $ makeObj [
          ("ObjType", showJSON $ show objType)
        , ("Mat", showJSON (unwords. lines $ show mat))
        ]
    readJSON (JSObject obj) = do
        objType <- obj ! "ObjType" :: Result String
        mat     <- obj ! "Mat"     :: Result String
        return $ TransformComponent (read objType) (read mat)
    readJSON _ = mzero

instance ComponentCreator TransformManager where
    createComponent goid objData (TransformManager mats grid) =
        let tc = readJSON objData :: Result TransformComponent
        in case tc of
            (Ok tc'@(TransformComponent _ mat)) -> let loc     = getGridXY mat
                                                       newGrid = updateGrid goid loc Insert grid
                                                   in Right $ TransformManager (Map.insert goid tc' mats) newGrid
            (Error err) -> error $ "creating transform component: " ++ err

    update _ = do
        evts <- getEventsFromInstance ["characterMoved", "death"]
        updateTransformManagerFromEvents evts

updateTransformManagerFromEvents :: [Event] -> Instance (Maybe String)
updateTransformManagerFromEvents [] = return Nothing
updateTransformManagerFromEvents (evt:evts) = do
    err <- case evt of
        (CharacterMovedEvent goid loc) -> do
            s <- get
            let (TransformManager mats _) = getManager Transform s
                mat = Map.lookup goid mats
            mat' <- case mat of
                    (Just (TransformComponent _ mat'')) -> return mat''
                    -- if we don't already have any information for this object, then make a new one and update it
                    -- will need to poll the server for data on this object since we don't have it yet
                    -- type information, etc.
                    Nothing -> return $ Mat.ident 4
            moveObject goid (mat' Mat.<> Mat.fromLists [loc])
        (DeathEvent goid) -> do
            s <- get
            let tm = getManager Transform s
            let tm' = tm { getMatrices = Map.delete goid (getMatrices tm)
                         , getGrid     = updateGrid goid (getObjectLoc goid tm) Delete (getGrid tm)
                         }
            put $ putManager Transform (ComponentManager tm') s
            return Nothing
        _ -> return Nothing
    err' <- updateTransformManagerFromEvents evts
    return $ (++) <$> err <*> err'

getGridXY :: Mat.Matrix Float -> (Int, Int)
getGridXY m = let x:y:_ = Mat.toLists $ m Mat.<> Mat.fromLists [[0],[0],[0],[1]]
              in (truncate (head x), truncate (head y))

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
                                        _ -> newGrid

getObjectLoc :: GOiD -> TransformManager ->  (Int, Int)
getObjectLoc goid (TransformManager mats _) = let (Just comp) = Map.lookup goid mats
                                              in getGridXY (getMatrix comp)

moveObject :: GOiD -> Mat.Matrix Float -> Instance (Maybe String)
moveObject  goid newLoc = do
    s <- get
    let tm@(TransformManager mats grid) = getManager Transform s
        obj = Map.lookup goid mats
    maybe (return . Just $ "there is no object with GOiD, " ++ show goid ++ ", that is able to be moved")
          (const $ let tc  = Map.lookup goid mats
                       loc = getGridXY newLoc
                       continueWithMovement (TransformComponent typ mat) =
                           let collisionIds = checkCollision loc tm
                               -- make sure to remove the object we're trying to
                               -- move so that it doesn't block itself if moving
                               -- to the same place
                           in if not . null $ filter (/= goid) collisionIds
                              then return . Just $ "location " ++ show loc ++ " is blocked for GOiD " ++ show goid ++ ", by GOiD's " ++ show collisionIds ++ "."
                              else let oldLoc = getGridXY mat
                                       gridWithOldDeleted = updateGrid goid oldLoc Delete grid
                                       grid' = updateGrid goid loc Insert gridWithOldDeleted
                                   in do
                                       let tm' = (getManager Transform s) { getMatrices = Map.update (\_ -> Just $ TransformComponent typ newLoc) goid mats
                                                                          , getGrid     = grid'
                                                                          }
                                       put $ putManager Transform (ComponentManager tm') s
                                       return Nothing
                   in maybe (return . Just $ "no matrix for component when moving; GOiD: " ++ show goid)
                            continueWithMovement
                            tc) obj

checkCollision :: (Int, Int) -> TransformManager -> [GOiD]
checkCollision loc tm = foldr checkBlocked [] (getObjectsAt loc tm)

checkBlocked :: (GOiD, TransformComponent) -> [GOiD] -> [GOiD]
checkBlocked (goid, TransformComponent Blocked _) acc = goid : acc
checkBlocked (_,    TransformComponent Open _)  acc = acc

getObjectsAt :: (Int, Int) -> TransformManager -> [(GOiD, TransformComponent)]
getObjectsAt loc (TransformManager mats grid) =
    let ids = Map.lookup loc grid
    in case ids of
        (Just ids') -> Map.toList $ Map.filterWithKey (\x _ -> x `elem` ids') mats
        _ -> []

getObjectMatrix :: GOiD -> TransformManager -> Mat.Matrix Float
getObjectMatrix goid (TransformManager mats _) = let mat = Map.lookup goid mats
                                                 in case mat of
                                                     (Just (TransformComponent _ mat')) -> mat'
                                                     _   -> error $ "no matrix for id " ++ show goid ++ "in getObjectMatrix"

data Direction = North | South | East | West
               | NWest | SWest
               | NEast | SEast
                 deriving (Show, Read)

getExits :: (Int, Int) -> TransformManager -> [Direction]
getExits (x,y) tm =
    let n = [North | null $ checkCollision (x + 1, y) tm]
        s = [South | null $ checkCollision (x - 1, y) tm]
        e = [East  | null $ checkCollision (x, y + 1) tm]
        w = [West  | null $ checkCollision (x, y - 1) tm]
    in n++s++e++w
