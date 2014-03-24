module Component.Manager.Ai
(
) where

import Text.JSON
import Control.Monad
import Control.Monad.Trans.State (state, get)
import Control.Applicative
import qualified Data.Map as Map

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
        return $ map (runAiComputers (Map.elems comps)) (Map.keys comps)
        return Nothing

runAiComputers :: [AiComputer] -> GOiD -> Instance ()
runAiComputers [] _ = return ()
runAiComputers (ac:acs) goid = do
    s <- get
    ac goid
    runAiComputers acs goid
    

getComputerFromJSON :: AiComponent -> AiComputer
getComputerFromJSON computerType = case computerType of
    Enemy     -> enemyComputer
    otherwise -> enemyComputer
    
enemyComputer :: AiComputer
enemyComputer _ = return ()
