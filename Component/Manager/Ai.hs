module Component.Manager.Ai
( AiManager(..)
, AiComponent(..)
, AiComputer
, getComputerFromJSON
) where

import Text.JSON
import Control.Monad
import Control.Monad.Trans.State
import Control.Applicative
import qualified Data.Map as Map
import qualified Numeric.Matrix as Mat
import Data.Function
import Debug.Trace

import Component
import Component.Manager.Transform
import Component.Manager.Character
import Common
import Math

data AiComponent = Enemy | Passive | Follow | Guard
                   deriving (Show, Read)

type AiComputer = GOiD -> InstanceState TransformManager CharacterManager AiManager -> AiManager

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
            put $ s { aiManager = AiManager $ Map.delete dead comps}
            return ()
 
        -- processedEvents must come first otherwise a 'dead' component would compute
        -- its ai unnecessarily
        foldr ((=<<) . const) (return ()) processedEvents

        s <- get
        let (AiManager comps) = aiManager s
        let s' = Map.foldlWithKey' (\acc id comp -> acc { aiManager = comp id acc} ) s comps
        trace (show s') (return ())
        put s'
        return Nothing

getComputerFromJSON :: AiComponent -> AiComputer
getComputerFromJSON computerType = case computerType of
    Enemy     -> enemyComputer
    Follow    -> followComputer
    Guard     -> guardComputer
    Passive   -> const aiManager

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
        when (isCharacter cm idOfNearby ||
              isCharacter cm thisId) $
            let char2 = getCharacter cm idOfNearby
                (Just thisChar) = getCharacter cm thisId
            in case char2 of
                Nothing       -> return ()
                (Just char2') -> void $ attackObject thisId idOfNearby (iShouldAttack thisChar char2')

guardComputer :: AiComputer
guardComputer thisId is = aiManager . flip execState is $
    -- lets look in all spaces within three moves of us
    attackClose thisId 3 $ \thisChar char2 ->
        if faction char2 /= faction thisChar
        then Torso
        else DontHit
enemyComputer :: AiComputer
enemyComputer thisId is = aiManager . flip execState is $
    -- lets look in all the spaces surrounding our location
    attackClose thisId 1 $ \thisChar char2 ->
        if health char2 <= health thisChar
        then Torso
        else DontHit

followComputer :: AiComputer
followComputer thisId is = aiManager . flip execState is $ do
    s <- get
    let player = Component.player s
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
            otherwise -> return ()
