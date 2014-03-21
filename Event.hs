module Event
( Event(..)
, EventDescriptor(..)
, JSONEvent(..)
, buildEventJSON
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

data Event = AttackEvent (GOiD, GOiD)
           | CharacterMovedEvent GOiD [Float]
           | RequestCharacterCreationEvent String [Float]
             deriving ( Show
                      )

instance JSON Event where
    readJSON = undefined
    showJSON (AttackEvent (char1, char2)) =
        buildEventJSON "attack" [("Char1", showJSON char1), ("Char2", showJSON char2)]
    showJSON (RequestCharacterCreationEvent charType loc) =
        buildEventJSON "requestCharacterCreation" [("Type", showJSON charType), ("Location", showJSON loc)]
    showJSON (CharacterMovedEvent char loc) =
        buildEventJSON "characterMoved" [("CharID", showJSON char), ("NewLocation", showJSON loc)]

instance JSONEvent Event where
    getEvent (EventDescriptor "attack" event) = 
        case readJSON event of
            (Ok (JSObject obj)) -> let (Ok char1) = obj ! "Char1"
                                       (Ok char2) = obj ! "Char2" 
                                   in AttackEvent (char1,char2)
    getEvent (EventDescriptor "requestCharacterCreation" event) =
        case readJSON event of
            (Ok (JSObject obj)) -> let (Ok goid) = obj ! "Type"
                                       (Ok loc)  = obj ! "Location"
                                   in RequestCharacterCreationEvent goid loc
    getEvent (EventDescriptor "characterMoved" event) = 
        case readJSON event of
            (Ok (JSObject obj)) -> let (Ok goid) = obj ! "CharID"
                                       (Ok loc)  = obj ! "NewLocation"
                                   in CharacterMovedEvent goid loc
             
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
