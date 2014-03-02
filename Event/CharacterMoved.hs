module Event.CharacterMoved
( CharacterMovedEvent(..)
) where

import Text.JSON

import Component
import Event

data CharacterMovedEvent = CharacterMovedEvent GOiD [Float]
                           deriving Show

instance JSON CharacterMovedEvent where
    readJSON = undefined
    showJSON (CharacterMovedEvent char loc) =
        buildEventJSON "characterMoved" [("CharID", showJSON char), ("NewLocation", showJSON loc)]

instance Event CharacterMovedEvent where
    getEvent (EventDescriptor "characterMoved" event) = 
        case readJSON event of
            (Ok (JSObject obj)) -> let (Ok goid) = obj ! "CharID"
                                       (Ok loc)  = obj ! "NewLocation"
                                   in CharacterMovedEvent goid loc
