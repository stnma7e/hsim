{-# LANGUAGE ExistentialQuantification #-}

module Component where

import Control.Monad.Trans.State
import Text.JSON
import Text.Show.Functions ()
import Control.Monad
import qualified Data.Map as Map
import qualified Numeric.Matrix as Mat
import System.Random
import Data.Maybe
import Control.Applicative
import Unsafe.Coerce

import Common
import Math

eventTypeAttack                   :: String
eventTypeAttack                   = "attack"
eventTypeDeath                    :: String
eventTypeDeath                    = "death"
eventTypeKill                     :: String
eventTypeKill                     = "kill"
eventTypeCharacterMoved           :: String
eventTypeCharacterMoved           = "characterMoved"
eventTypeRequestCharacterCreation :: String
eventTypeRequestCharacterCreation = "requestCharacterCreation"

type GOiD = Int

data GameObjectJSON = GameObjectJSON
    { transform :: JSValue
    , character :: JSValue
    , ai        :: JSValue
    }
instance JSON GameObjectJSON where
    showJSON = undefined
    readJSON (JSObject obj) = do
        tm <- obj ! "Transform" :: Result JSValue
        cm <- obj ! "Character" :: Result JSValue
        am <- obj ! "Ai"        :: Result JSValue
        return $ GameObjectJSON tm cm am
    readJSON _ = mzero

buildObjectJSON :: (JSON a, JSON b, JSON c) => a -> b -> c -> JSValue
buildObjectJSON tm cm am = showJSON $ makeObj [ ("Transform", showJSON tm)
                                              , ("Character", showJSON cm)
                                              , ("Ai", showJSON am)
                                              ]

data Event = AttackEvent (GOiD, GOiD) Int
           | DeathEvent GOiD
           | KillEvent GOiD GOiD
           | CharacterMovedEvent GOiD [Float]
           | RequestCharacterCreationEvent String [Float]
             deriving (Show, Read, Eq)

instance JSON Event where
    readJSON = undefined
    showJSON (AttackEvent (char1, char2) damage) =
        buildEventJSON eventTypeAttack [("Char1", showJSON char1), ("Char2", showJSON char2), ("Damage", showJSON damage)]
    showJSON (RequestCharacterCreationEvent charType loc) =
        buildEventJSON eventTypeRequestCharacterCreation [("Type", showJSON charType), ("Location", showJSON loc)]
    showJSON (CharacterMovedEvent char loc) =
        buildEventJSON eventTypeCharacterMoved [("CharID", showJSON char), ("NewLocation", showJSON loc)]
    showJSON (DeathEvent char) =
        buildEventJSON eventTypeCharacterMoved [("Dead", showJSON char)]
    showJSON (KillEvent killer dead) =
        buildEventJSON eventTypeCharacterMoved [("KilledBy", showJSON killer), ("Dead", showJSON dead)]

jsonTypeField :: String
jsonTypeField  = "Type"
jsonEventField :: String
jsonEventField = "Event"

buildEventJSON :: String -> [(String, JSValue)] -> JSValue
buildEventJSON typ event = showJSON $ makeObj [(jsonTypeField, showJSON typ), (jsonEventField, makeObj event)]

getEventsFromInstance :: [String] -> Instance [Event]
getEventsFromInstance [] =  do
    s <- get
    return . join . map snd $ Map.toList (fst $ getEvents s)
getEventsFromInstance eventsToLookFor = do
        s <- get
        -- lets get a list of all the events we're going to look at
        let evts = map (`Map.lookup` (fst $ getEvents s)) eventsToLookFor
        -- then filter out all of the either empty lists or nonexistent event types (a.k.a values constructed with Nothing)
        return . join $ filter (not . null) [fromJust x | x <- evts, isJust x]

pushEvent :: Event -> Instance ()
pushEvent evtToBeInserted = insertEvent evtToBeInserted $ case evtToBeInserted of
        (AttackEvent _ _)  -> eventTypeAttack
        (DeathEvent _)     -> eventTypeDeath
        (KillEvent _ _)    -> eventTypeKill
        _ -> ""
    where insertEvent :: Event -> String -> Instance ()
          insertEvent evt typ = state $ \s ->
              let (currentFrameEvents, nextFrameEvents) = getEvents s
                  eventsOfCurrentType = Map.lookup typ nextFrameEvents
                  newEventList = case eventsOfCurrentType of
                      (Just curEvts) -> Map.insert typ (evt : curEvts) nextFrameEvents
                      _              -> Map.insert typ [evt] nextFrameEvents
              in if null typ
                 then ((), s)
                 else ((), s { getEvents = (currentFrameEvents, newEventList) })

class ComponentCreator a where
    createComponent :: GOiD -> JSValue -> a -> Either String a
    update :: a -> Instance (Maybe String)

type EventList = Map.Map String [Event]

data ComponentManager = forall a . (ComponentCreator a, Show a) => ComponentManager a
instance Show ComponentManager where
    show (ComponentManager a) = show a

data ComponentType = Transform
                   | Character
                   | Ai
                     deriving (Show, Eq)

type Instance = State InstanceState
data InstanceState = InstanceState
    { getInstancePlayer :: GOiD
    , getEvents         :: (EventList, EventList)
    , availiableIDS     :: [GOiD]
    , randomNumGen      :: StdGen
    , managers          :: [(ComponentType, ComponentManager)]
    } deriving Show

putManager :: ComponentType -> ComponentManager -> InstanceState -> InstanceState
putManager typ manager is = is { managers = replace (typ, manager) [] (managers is) }
    where replace :: (ComponentType, ComponentManager) -> [(ComponentType, ComponentManager)] -> [(ComponentType, ComponentManager)] -> [(ComponentType, ComponentManager)]
          replace man ms [] = ms ++ [man]
          replace man mss (m:ms) = if fst m == fst man
                                       then mss ++ man:ms
                                       else replace man (mss ++ [m]) ms

transformManager :: InstanceState -> TransformManager
transformManager is = case lookup Transform $ managers is of
                          (Just (ComponentManager a)) -> unsafeCoerce a
                          _ -> error "here3"

characterManager :: InstanceState -> CharacterManager
characterManager is = case lookup Character $ managers is of
                          (Just (ComponentManager a)) -> unsafeCoerce a
                          _ -> error "here3"

aiManager :: InstanceState -> AiManager
aiManager is = case lookup Ai $ managers is of
                   (Just (ComponentManager a)) -> unsafeCoerce a
                   _ -> error "here3"

-------------------------------------
-- Managers --
-------------------------------------

--
-- Transform
--

data ObjectType = Blocked | Open
                  deriving (Show , Read, Eq)

data TransformComponent = TransformComponent
    { getObjType   :: ObjectType
    , getMatrix :: Mat.Matrix Float
    } deriving (Show, Eq)

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
        let mats  = map read $ words mat
            -- split the joined list into 4 rows for the matrix
        let (mats1, matsx1) = splitAt 4 mats
        let (mats2, matsx2) = splitAt 4 matsx1
        let (mats3, matsx3) = splitAt 4 matsx2
        let mats' = [mats1, mats2, mats3, matsx3]
        return $ TransformComponent (read objType) (Mat.fromList mats')
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
            let (TransformManager mats _) = transformManager s
                mat = Map.lookup goid mats
            mat' <- case mat of
                    (Just (TransformComponent _ mat'')) -> return mat''
                    -- if we don't already have any information for this object, then make a new one and update it
                    -- will need to poll the server for data on this object since we don't have it yet
                    -- type information, etc.
                    Nothing -> return $ Mat.unit 4
            moveObject goid (mat' `Mat.times` Mat.fromList [loc])
        (DeathEvent goid) -> do
            s <- get
            let tm = transformManager s
            let tm' = tm { getMatrices = Map.delete goid (getMatrices tm)
                         , getGrid     = updateGrid goid (getObjectLoc goid tm) Delete (getGrid tm)
                         }
            put $ putManager Transform (ComponentManager tm') s
            return Nothing
        _ -> return Nothing
    err' <- updateTransformManagerFromEvents evts
    return $ (++) <$> err <*> err'

getGridXY :: Mat.Matrix Float -> (Int, Int)
getGridXY m = let x:y:_ = Mat.toList $ m `Mat.times` Mat.fromList [[0],[0],[0],[1]]
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
    let tm@(TransformManager mats grid) = transformManager s
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
                                       let tm' = (transformManager s) { getMatrices = Map.update (\_ -> Just $ TransformComponent typ newLoc) goid mats
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
                                                     _   -> error $ "no matrix for id " ++ show id ++ "in getObjectMatrix"

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

--
-- Character
--

data HitLocation = DontHit
                 | Head | Torso | Legs
                   deriving (Show, Read, Eq)

data Faction = Betuol | Dunteg | Blitzal
               deriving (Show , Read, Eq, Ord)

data SpellType = Melee | Fire | Earth | Frost | Air
                 deriving (Show, Read, Eq)

data DamageType = DamageType Float [SpellType]
                 deriving (Show, Read, Eq)

type Reputation = (Faction, Int)

data CharacterEquipment = EmptyEquipment
                        | CharacterEquipment
    { weapon :: DamageType
    } deriving (Show, Read, Eq)

data CharacterComponent = CharacterComponent
    { getCharHealth  :: Int
    , getCharMana    :: Int
    , getCharFaction :: Faction
    , getCharRep     :: [Reputation]
    , getCharEquipment :: CharacterEquipment
    } deriving (Show, Eq)

newtype CharacterManager = CharacterManager (Map.Map GOiD CharacterComponent)
                           deriving Show

getCharDamage :: CharacterComponent -> Float
getCharDamage char = case getCharEquipment char of
    (CharacterEquipment ce) -> let (DamageType damageOfAttack _) = ce
                               in damageOfAttack
    _ -> 0

instance JSON CharacterComponent where
    showJSON char = showJSON $ makeObj [
          ("Health",     showJSON $ getCharHealth           char)
        , ("Mana",       showJSON $ getCharMana             char)
        , ("Faction",    showJSON . show $ getCharFaction   char)
        , ("Reputation", showJSON . show $ getCharRep       char)
        , ("Equipment",  showJSON . show $ getCharEquipment char)
        ]
    readJSON (JSObject obj) = do
        charHealth  <- obj ! "Health"     :: Result Int
        charMana    <- obj ! "Mana"       :: Result Int
        charFaction <- obj ! "Faction"    :: Result String
        charRep     <- obj ! "Reputation" :: Result String
        charEquip   <- obj ! "Equipment"  :: Result String
        return $ CharacterComponent charHealth charMana (read charFaction) (read charRep) (read charEquip)
    readJSON _ = mzero

instance ComponentCreator CharacterManager where
    createComponent goid objData (CharacterManager ids) =
        let cc = readJSON objData :: Result CharacterComponent
        in case cc of
            (Ok cc')-> Right . CharacterManager $ Map.insert goid cc' ids
            (Error err) -> error $ "creating character component: " ++ err
    update _ = do
        evts <- getEventsFromInstance ["attack", "death"]
        updateCharacterManagerFromEvents evts

        (CharacterManager ids) <- liftM characterManager get
        -- the deleted components from earlier events won't be counted again
        -- because we already did our event update for last frame
        let deadIds = Map.foldrWithKey (\goid comp acc -> if getCharHealth comp <= 0 then goid:acc else acc) []  ids
        foldr ((\is acc -> acc >>= const is) . pushEvent . DeathEvent) (return ()) deadIds
        return Nothing

updateCharacterManagerFromEvents :: [Event] -> Instance ()
updateCharacterManagerFromEvents [] = return ()
updateCharacterManagerFromEvents (evt:evts) = do
    case evt of
        (DeathEvent goid) -> do
            s <- get
            let (CharacterManager ids) = characterManager s
                cm = CharacterManager $ Map.delete goid ids
            put $ putManager Character (ComponentManager cm) s

            return ()
        _ -> return ()

    updateCharacterManagerFromEvents evts

data AttackType = Hit Int | Miss
                  deriving (Show, Read, Eq)

attackObject :: GOiD -> GOiD -> HitLocation -> Instance AttackType
attackObject id1 id2 hitLoc = do
    s <- get
    let (CharacterManager ids) = characterManager s
        char1 = Map.lookup id1 ids
        char2 = Map.lookup id2 ids
    if isNothing char1 || isNothing char2 || hitLoc == DontHit
    then return Miss
    else let (Just char1') = char1
             (Just char2') = char2
             (hitMiss, getCharHealth2', newGen) = attackComponent (getCharHealth char1', getCharHealth char2')
                                                           (DamageType (getCharDamage char1') [Melee])
                                                           hitLoc
                                                           (randomNumGen s)
             (Just rep1) = lookup (getCharFaction char1') (getCharRep char1')
             reputationDiff = if getCharFaction char2' == getCharFaction char2'
                                then -1
                                else  1
             ids' = Map.update (const $ Just char2' { getCharHealth = getCharHealth2'
                                                    , getCharRep    = replaceReputation (getCharFaction char1', rep1 + reputationDiff) (getCharRep char1') []
                                                    }) id2 ids
         in if getCharHealth char2' <= 0
            then return Miss
            else do
                when (getCharHealth2' <= 0) $
                    pushEvent (KillEvent id1 id2)

                pushEvent (AttackEvent (id1, id2) (getCharHealth char2' - getCharHealth2'))
                s' <- get
                put $ (putManager Character (ComponentManager $ CharacterManager ids') s') { randomNumGen = newGen
                                                                                           }
                return hitMiss

replaceReputation :: Reputation -> [Reputation] -> [Reputation] -> [Reputation]
replaceReputation _ [] rs = rs
replaceReputation f@(fac, _) (f'@(fac', _):fx) rs = if fac' == fac
                                            then f:fx
                                            else replaceReputation f fx (rs ++ [f'])

type Health = Int
attackComponent :: (Health, Health) -> DamageType -> HitLocation -> StdGen -> (AttackType, Health, StdGen)
attackComponent (health1, health2) (DamageType damage1 _) hitLoc rnd =
    let (rndNum, newGen) = randomR (1, 100) rnd :: (Int, StdGen)
        damageDealt1 = truncate $ case hitLoc of
            Head    -> if rndNum > 10 then 0 else damage1 * 2
            Torso   -> if rndNum > 90 then 0 else damage1
            Legs    -> if rndNum > 70 then 0 else damage1 * 1.5
            DontHit -> 0
        hitMiss = if damageDealt1 > 0
                  then Hit damageDealt1
                  else Miss
    in if health2 <= 0 || health1 <= 0
         then (Miss, health2, newGen)
         else (hitMiss, health2 - damageDealt1, newGen)

isCharacter :: CharacterManager -> GOiD -> Bool
isCharacter (CharacterManager ids) = flip Map.member ids

getCharacter :: CharacterManager -> GOiD -> Maybe CharacterComponent
getCharacter (CharacterManager ids) = flip Map.lookup ids

--
-- AI
--

data AiComponent = Enemy | Passive | Follow | Guard
                   deriving (Show, Read)

type AiComputer = GOiD -> Instance ()

newtype AiManager = AiManager (Map.Map GOiD AiComputer)
                    deriving Show

instance JSON AiComponent where
    showJSON computerType = showJSON $ makeObj
        [ ("Computer", showJSON $ show computerType)
        ]
    readJSON (JSObject obj) = do
        computer <- obj ! "Computer" :: Result String
        return $ read computer
    readJSON _ = mzero

instance ComponentCreator AiManager where
    createComponent goid objData (AiManager comps) =
        let ac = readJSON objData :: Result AiComponent
        in case ac of
            (Ok ac')-> Right . AiManager $ Map.insert goid (getComputerFromJSON ac') comps
            (Error err) -> error $ "creating ai component: " ++ err

    update _ = do
        -- we'll get a list of all the `death` events from the instance
        evts <- getEventsFromInstance ["death"]
        -- then assign a function to process these
        -- and store these computation thunks in a list
        -- the list will get processed later on with the list of ai computations
        let processedEvents = flip map evts $ \(DeathEvent dead) -> do
            -- deleting their ai functions so they won't get computed each frame
            s <- get
            let (AiManager comps) = aiManager s
            put $ putManager Ai (ComponentManager . AiManager $ Map.delete dead comps) s
            return ()

        -- processedEvents must come first otherwise a 'dead' component would compute
        -- its ai unnecessarily
        foldr ((=<<) . const) (return ()) processedEvents

        s <- get
        let (AiManager comps) = aiManager s
        Map.foldrWithKey (\goid comp acc -> acc >> comp goid) (return ()) comps
        return Nothing

getComputerFromJSON :: AiComponent -> AiComputer
getComputerFromJSON computerType = case computerType of
    Enemy     -> enemyComputer
    Follow    -> followComputer
    Guard     -> guardComputer
    Passive   -> \_ -> return ()

attackClose :: GOiD -> Int -> (CharacterComponent -> CharacterComponent -> HitLocation) -> Instance ()
attackClose thisId radiusToLook iShouldAttack = do
    s <- get
    let tm = transformManager s
        cm = characterManager s
        thisLoc = getObjectLoc thisId tm
        placesToLook = zipWith (\(i1, i1') (i2, i2') -> (i1 + i2, i1' + i2')) (repeat thisLoc) $
            map (\(i1, i2) -> (i1 * radiusToLook, i2 * radiusToLook))
                [(0,0), (-1,1), (0,1), (1,1), (-1,0), (1,0), (0,-1), (-1,-1), (1,-1)]
        closeObjects = filter (/= thisId) . filter (isCharacter cm) . map fst . join $ map (flip getObjectsAt tm) placesToLook

    foldr (\is acc -> acc >>= const is) (return ()) $ flip map closeObjects $ \idOfNearby ->
    -- begin ai computation
        when (isCharacter cm idOfNearby ||
              isCharacter cm thisId) $
            let char2 = getCharacter cm idOfNearby
                (Just thisChar) = getCharacter cm thisId
            in case char2 of
                Nothing       -> return ()
                (Just char2') -> attackObject thisId idOfNearby (iShouldAttack thisChar char2')
                                 >> return ()

guardComputer :: AiComputer
guardComputer thisId =
    attackClose thisId 3 $ \thisChar char2 ->
        if getCharFaction char2 /= getCharFaction thisChar
        then Torso
        else DontHit

enemyComputer :: AiComputer
enemyComputer thisId =
    -- lets look in all the spaces surrounding our location
    attackClose thisId 1 $ \thisChar char2 ->
        if getCharHealth char2 <= getCharHealth thisChar
        then Torso
        else DontHit

followComputer :: AiComputer
followComputer thisId = do
    s <- get
    let player = (getInstancePlayer s)
        tm = transformManager s
    unless (getObjectLoc player tm == getObjectLoc thisId tm) $ do
        let playerLoc   = getObjectMatrix player tm `Mat.times` Mat.fromList [[0],[0],[0],[1]]
            computerMatrix = getObjectMatrix thisId tm
            computerLoc = computerMatrix `Mat.times` Mat.fromList [[0],[0],[0],[1]]
            diffVector  = playerLoc `Mat.minus` computerLoc
            translationMat = buildTranslationMatrix (4,4) (concat $ Mat.toList diffVector) `Mat.times` computerMatrix
        err <- moveObject thisId translationMat
        case err of
            (Just err') -> error err'
            _ -> return ()
