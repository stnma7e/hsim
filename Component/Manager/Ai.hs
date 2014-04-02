module Component.Manager.Ai
( getComputerFromJSON
) where

import Text.JSON
import Control.Monad
import Control.Monad.Trans.State (state, get, put)
import Control.Applicative
import qualified Data.Map as Map
import qualified Numeric.Matrix as Mat
import Data.Function

import Component.Manager.Transform
import Component.Manager.Character
import Component
import Common
import Math

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
        Map.foldrWithKey (\id comp acc -> acc >> comp id) (return ()) comps
        return Nothing

getComputerFromJSON :: AiComponent -> AiComputer
getComputerFromJSON computerType = case computerType of
    Enemy     -> enemyComputer
    Follow    -> followComputer
    Passive   -> \_ -> return ()
    
enemyComputer :: AiComputer
enemyComputer thisId = do
    s <- get
    let tm = transformManager s
        cm = characterManager s
    let closeObjects = filter (isCharacter cm) . map fst $ getObjectsAt (getObjectLoc thisId tm) tm
    foldr (\is acc -> acc >>= const is) (return ()) $ flip map closeObjects $ \idOfNearby ->
    -- begin ai computation
        unless (not (isCharacter cm idOfNearby) ||
                not (isCharacter cm thisId)     ||
                thisId == idOfNearby) $ do
            let char1 = getCharacter cm idOfNearby
                (Just thisChar) = getCharacter cm thisId
            case char1 of
                Nothing       -> return ()
                (Just char1') -> when (health char1' <= health thisChar) $
                                     void $ attackObject thisId idOfNearby Torso

followComputer :: AiComputer
followComputer thisId = do
    s <- get
    let player = (Component.player s)
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
