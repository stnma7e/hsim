module Component.Manager.Ai
(
) where

import Text.JSON
import Control.Monad
import Control.Monad.Trans.State (state, get, put)
import Control.Applicative
import qualified Data.Map as Map

import Component.Manager.Transform
import Component.Manager.Character
import Component
import Common

instance JSON AiComponent where
    showJSON computerType = showJSON $ makeObj [
          ("Computer", showJSON $ show computerType)
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
        s <- get
        let (AiManager comps) = aiManager s
        foldr (\is acc -> acc >>= const is) (return ()) $ map (runAiComputers (Map.elems comps)) (Map.keys comps)
        return Nothing

runAiComputers :: [AiComputer] -> GOiD -> Instance ()
runAiComputers [] _ = return ()
runAiComputers (ac:acs) goid = do
    ac goid
    runAiComputers acs goid
    

getComputerFromJSON :: AiComponent -> AiComputer
getComputerFromJSON computerType = case computerType of
    Enemy     -> enemyComputer
    Passive   -> passiveComputer
    
passiveComputer :: AiComputer
passiveComputer _ = return ()

enemyComputer :: AiComputer
enemyComputer thisId = do
    s <- get
    let tm = transformManager s
        cm = characterManager s
    let closeObjects = filter (isCharacter cm) . map fst $ getObjectsAt (getObjectLoc thisId tm) tm
    foldr (\is acc -> acc >>= const is) (return ()) $ flip map closeObjects (\idOfNearby ->
    -- end boilerplate
    -- begin ai computation
        if (not $ isCharacter cm idOfNearby) ||
           (not $ isCharacter cm thisId)     ||
           thisId == idOfNearby
        then return ()
        else do
            let char1 = getCharacter cm idOfNearby
                (Just thisChar) = getCharacter cm thisId
            case char1 of
                Nothing       -> return ()
                (Just char1') -> if (health char1') <= (health thisChar)
                                 then do
                                     attackObject thisId idOfNearby Torso
                                     return ()
                                 else return ())
    return ()
