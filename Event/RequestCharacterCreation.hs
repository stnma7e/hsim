module Event.RequestCharacterCreation
( RequestCharacterCreationEvent(..)
) where

import Event
import Text.JSON

data RequestCharacterCreationEvent = RequestCharacterCreationEvent String [Float]
    deriving Show

instance JSON RequestCharacterCreationEvent where
    readJSON = undefined
    showJSON (RequestCharacterCreationEvent charType loc) = buildEventJSON "requestCharacterCreation" [("Type", showJSON charType), ("Location", showJSON loc)]

instance Event RequestCharacterCreationEvent where
    getEvent (EventDescriptor "requestCharacterCreation" event) =
        case readJSON event of
            (Ok (JSObject obj)) -> let (Ok goid) = obj ! "Type"
                                       (Ok loc)  = obj ! "Location"
                                   in RequestCharacterCreationEvent goid loc
