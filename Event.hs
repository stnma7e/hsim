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

class (JSON a, Show a) => JSONEvent a where
    getEvent :: EventDescriptor -> a

instance JSONEvent Event where
    getEvent (EventDescriptor eventType event) 
        | eventType == eventTypeAttack =
            case readJSON event of
                (Ok (JSObject obj)) -> let (Ok char1)  = obj ! "Char1"
                                           (Ok char2)  = obj ! "Char2" 
                                           (Ok damage) = obj ! "Damage"
                                       in AttackEvent (char1,char2) damage
                _ -> error "cannot read attack event from data"
        | eventType == eventTypeRequestCharacterCreation =
            case readJSON event of
                (Ok (JSObject obj)) -> let (Ok goid) = obj ! "Type"
                                           (Ok loc)  = obj ! "Location"
                                       in RequestCharacterCreationEvent goid loc
                _ -> error "cannot read attack event from data"
        | eventType == eventTypeCharacterMoved =
            case readJSON event of
                (Ok (JSObject obj)) -> let (Ok goid) = obj ! "CharID"
                                           (Ok loc)  = obj ! "NewLocation"
                                       in CharacterMovedEvent goid loc
                _ -> error "cannot read attack event from data"
        | eventType == eventTypeDeath =
            case readJSON event of
                (Ok (JSObject obj)) -> let (Ok dead)     = obj ! "Dead"
                                       in DeathEvent dead
                _ -> error "cannot read attack event from data"
        | eventType == eventTypeKill =
            case readJSON event of
                (Ok (JSObject obj)) -> let (Ok dead)     = obj ! "Dead"
                                           (Ok killedBy) = obj ! "KilledBy"
                                       in KillEvent killedBy dead
                _ -> error "cannot read attack event from data"
        | otherwise = error "no event type for event data"
             
data EventDescriptor = EventDescriptor
    { eventDescriptionType :: String
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
