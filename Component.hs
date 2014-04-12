{-# LANGUAGE ExistentialQuantification #-}

module Component where

import Control.Monad.Trans.State
import Control.Monad
import System.Random
import qualified Data.Map as Map
import Data.Maybe
import Text.JSON
import Unsafe.Coerce

import Common

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
             deriving (Show, Read, Eq)

instance JSON Event where
    readJSON = undefined
    showJSON (AttackEvent (char1, char2) damage) =
        buildEventJSON eventTypeAttack [("Char1", showJSON char1), ("Char2", showJSON char2), ("Damage", showJSON damage)]
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

data ComponentManager = forall a. (ComponentCreator a, Show a) => ComponentManager a
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
    where replace :: (ComponentType, ComponentManager)
                  -> [(ComponentType, ComponentManager)]
                  -> [(ComponentType, ComponentManager)]
                  -> [(ComponentType, ComponentManager)]
          replace man ms [] = ms ++ [man]
          replace man mss (m:ms) = if fst m == fst man
                                       then mss ++ man:ms
                                       else replace man (mss ++ [m]) ms

getManager :: ComponentType -> InstanceState -> a
getManager typ is = case lookup typ $ managers is of
                        (Just (ComponentManager a)) -> unsafeCoerce a
                        _ -> error "here3"
