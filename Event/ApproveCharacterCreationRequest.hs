module Event.ApproveCharacterCreationRequest
( ApproveCharacterCreationRequestEvent(..)
) where

import Instance
import Event
import Text.JSON

data ApproveCharacterCreationRequestEvent = ApproveCharacterCreationRequestEvent GOiD
    deriving Show

instance JSON ApproveCharacterCreationRequestEvent where
    readJSON = undefined
    showJSON (ApproveCharacterCreationRequestEvent goid) = buildEventJSON "approveCharacterCreationRequest" [("ID", showJSON goid)]

instance Event ApproveCharacterCreationRequestEvent where
    getEvent (EventDescriptor "approveCharacterCreationRequest" event) =
        case readJSON event of
            (Ok (JSObject obj)) -> let (Ok goid) = obj ! "ID"
                                   in ApproveCharacterCreationRequestEvent goid

