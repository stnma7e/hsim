module Event
( Event(..)
, EventDescriptor(..)
, JSONEvent(..)
, buildEventJSON

, eventTypeAttack
, eventTypeCharacterMoved
, eventTypeRequestCharacterCreation
) where

import Control.Monad
import Control.Applicative
import Text.JSON

import Common
import Component

jsonTypeField  = "Type"
jsonEventField = "Event"

class (JSON a, Show a) => JSONEvent a where
    getEvent :: EventDescriptor -> a

buildEventJSON :: String -> [(String, JSValue)] -> JSValue
buildEventJSON typ event = showJSON $ makeObj [(jsonTypeField, showJSON typ), (jsonEventField, makeObj event)]

instance JSON Event where
    readJSON = undefined
    showJSON (AttackEvent (char1, char2)) =
        buildEventJSON eventTypeAttack [("Char1", showJSON char1), ("Char2", showJSON char2)]
    showJSON (RequestCharacterCreationEvent charType loc) =
        buildEventJSON eventTypeRequestCharacterCreation [("Type", showJSON charType), ("Location", showJSON loc)]
    showJSON (CharacterMovedEvent char loc) =
        buildEventJSON eventTypeCharacterMoved [("CharID", showJSON char), ("NewLocation", showJSON loc)]
    showJSON (DeathEvent char) =
        buildEventJSON eventTypeCharacterMoved [("Dead", showJSON char)]
    showJSON (KillEvent killer dead) =
        buildEventJSON eventTypeCharacterMoved [("KilledBy", showJSON killer), ("Dead", showJSON dead)]

instance JSONEvent Event where
    getEvent (EventDescriptor eventType event) 
        | eventType == eventTypeAttack = 
            case readJSON event of
                (Ok (JSObject obj)) -> let (Ok char1) = obj ! "Char1"
                                           (Ok char2) = obj ! "Char2" 
                                       in AttackEvent (char1,char2)
        | eventType == eventTypeRequestCharacterCreation =
            case readJSON event of
                (Ok (JSObject obj)) -> let (Ok goid) = obj ! "Type"
                                           (Ok loc)  = obj ! "Location"
                                       in RequestCharacterCreationEvent goid loc | eventType == eventTypeCharacterMoved =
            case readJSON event of
                (Ok (JSObject obj)) -> let (Ok goid) = obj ! "CharID"
                                           (Ok loc)  = obj ! "NewLocation"
                                       in CharacterMovedEvent goid loc
        | eventType == eventTypeDeath =
            case readJSON event of
                (Ok (JSObject obj)) -> let (Ok dead)     = obj ! "Dead"
                                       in DeathEvent dead
        | eventType == eventTypeKill =
            case readJSON event of
                (Ok (JSObject obj)) -> let (Ok dead)     = obj ! "Dead"
                                           (Ok killedBy) = obj ! "KilledBy"
                                       in KillEvent killedBy dead
             
data EventDescriptor = EventDescriptor
    { eventType :: String
    , eventData :: JSValue
    } deriving (Show)

instance JSON EventDescriptor where
    showJSON = undefined
    readJSON (JSObject obj) = 
        let event = fmap (map toEnum) (obj ! jsonEventField :: Result [Int]) :: Result String
        in case event of
            (Ok event') -> EventDescriptor   <$>
                           obj ! jsonTypeField <*>
                           decode event'
            (Error err) -> error err
    readJSON _ = mzero
