module Component
where

import Control.Monad.Trans.State
import Text.JSON
import Text.Show.Functions
import Control.Monad
import qualified Data.Map as Map
import qualified Numeric.Matrix as Mat
import System.Random
import Data.Maybe

import Common
import {-# SOURCE #-} Component.Manager.Transform
import {-# SOURCE #-} Component.Manager.Character
import {-# SOURCE #-} Component.Manager.Ai

eventTypeAttack                   = "attack"
eventTypeDeath                    = "death"
eventTypeKill                     = "kill"
eventTypeCharacterMoved           = "characterMoved"
eventTypeRequestCharacterCreation = "requestCharacterCreation"

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

getEventsFromInstance :: [String] -> Instance [Event]
getEventsFromInstance [] =  do
    -- if the list of types to get is empty
    -- then we will return all events
    s <- get
    return . join . map snd $ Map.toList (fst $ getEvents s)
getEventsFromInstance eventsToLookFor = do
        s <- get
        -- lets get a list of all the events we're going to look at
        let evts = map (`Map.lookup` (fst $ getEvents s)) eventsToLookFor
        -- then filter out all of the either empty lists or nonexistent event types (a.k.a values constructed with Nothing)
        return . join $ filter (not . null) [fromJust x | x <- evts, isJust x]

pushEvent :: Event -> Instance ()
pushEvent evt = insertEvent evt $ case evt of
        (AttackEvent _ _)  -> eventTypeAttack
        (DeathEvent _)     -> eventTypeDeath
        (KillEvent _ _)    -> eventTypeKill
    where insertEvent :: Event -> String -> Instance ()
          insertEvent evt typ = state $ \s ->
              let (currentFrameEvents, nextFrameEvents) = getEvents s
                  eventsOfCurrentType = Map.lookup typ nextFrameEvents
                  newEventList = case eventsOfCurrentType of
                      (Just curEvts) -> Map.insert typ (evt : curEvts) nextFrameEvents
                      otherwise      -> Map.insert typ [evt] nextFrameEvents
              in ((), s { getEvents = (currentFrameEvents, newEventList) })

class ComponentCreator a where
    createComponent :: GOiD -> JSValue -> a -> Either String a
    update :: a -> Instance (Maybe String)

type EventList = Map.Map String [Event]

type Instance = State (InstanceState TransformManager CharacterManager AiManager)
data InstanceState a b c = InstanceState
    { player           :: GOiD
    , transformManager :: a
    , characterManager :: b
    , aiManager        :: c
    , getEvents        :: (EventList, EventList)
    , availiableIDS    :: [GOiD]
    , randomNumGen     :: StdGen
    } deriving Show
