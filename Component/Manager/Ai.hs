module Component.Manager.Ai
( runAiComputers
, getComputerFromJSON
) where

import Text.JSON
import Control.Monad
import Control.Monad.Trans.State (state, get, put)
import Control.Applicative
import qualified Data.Map as Map
import Data.Function

import Component.Manager.Transform
import Component.Manager.Character
import Component
import Common

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
        let processedEvents = flip map evts $ \(DeathEvent _ dead) -> do
            -- deleting their ai functions so they won't get computed each frame
            s <- get
            let (AiManager comps) = aiManager s
            put $ s { aiManager = AiManager $ Map.delete dead comps}
            return ()

        s <- get
        let (AiManager comps) = aiManager s
        let computationsToRun = map runAiComputers (Map.keys comps) <*> [Map.elems comps]
        -- processedEvents must come last in the list because we are folding from the right
        -- otherwise a 'dead' component would compute its ai unessecarially
        foldr (\is acc -> acc >>= const is) (return ()) (computationsToRun ++ processedEvents)
        return Nothing

runAiComputers :: GOiD -> [AiComputer] -> Instance ()
runAiComputers goid = fix (\f acs'@(ac:acs) -> ac goid >> unless (null acs) (f acs))
    
getComputerFromJSON :: AiComponent -> AiComputer
getComputerFromJSON computerType = case computerType of
    Enemy     -> enemyComputer
    Passive   -> \_ -> return ()
    
enemyComputer :: AiComputer
enemyComputer thisId = do
    s <- get
    let tm = transformManager s
        cm = characterManager s
    let closeObjects = filter (isCharacter cm) . map fst $ getObjectsAt (getObjectLoc thisId tm) tm
    foldr (\is acc -> acc >>= const is) (return ()) $ flip map closeObjects $ \idOfNearby ->
    -- begin ai computation
        if not (isCharacter cm idOfNearby) ||
           not (isCharacter cm thisId)     ||
           thisId == idOfNearby
        then return ()
        else do
            let char1 = getCharacter cm idOfNearby
                (Just thisChar) = getCharacter cm thisId
            case char1 of
                Nothing       -> return ()
                (Just char1') -> if health char1' <= health thisChar
                                 then void $ attackObject thisId idOfNearby Torso
                                 else return ()
