module Component.Manager.Ai
( AiManager(..)
, AiComponent(..)
, aiManager
) where

import Control.Monad.Trans.State
import Text.JSON
import Text.Show.Functions ()
import Control.Monad
import Unsafe.Coerce
import qualified Data.Map as Map
import qualified Numeric.Matrix as Mat

import Common
import Math
import Component
import Component.Manager.Transform
import Component.Manager.Character

aiManager :: InstanceState -> AiManager
aiManager is = case lookup Ai $ managers is of
                   (Just (ComponentManager a)) -> unsafeCoerce a
                   _ -> error "here3"

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
