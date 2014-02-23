module Event
( Event(..)
, getEvent
, sendEvent
) where

import Control.Monad
import Control.Applicative
import Control.Monad.Trans
import System.IO

import Network.Socket hiding (recv, sendAll)
import Network.Socket.ByteString (recv, sendAll)
import Data.ByteString.Char8 as C (unpack, pack)

import Text.JSON
import Data.List
import Data.Word

import Instance

jsonTypeField  = "Type"
jsonEventField = "Event"

data Event = AttackEvent (GOiD, GOiD)
           | CharacterMovedEvent GOiD [Float]
           | RequestCharacterCreationEvent String [Float]
           | ApproveCharacterCreationRequestEvent GOiD
           | CharacterCreatedEvent GOiD
             deriving (Show)

instance JSON Event where
    readJSON = undefined
    showJSON (AttackEvent (char1, char2)) = buildEventJSON "attack" [("Char1", showJSON char1), ("Char2", showJSON char2)]
    showJSON (CharacterMovedEvent char loc) = buildEventJSON "characterMoved" [("CharID", showJSON char), ("NewLocation", showJSON loc)]
    showJSON (RequestCharacterCreationEvent charType loc) = buildEventJSON "requestCharacterCreation" [("Type", showJSON charType), ("Location", showJSON loc)]
    showJSON evt = error $ "ERROR: no instance showJSON for type: " ++ (show evt)

buildEventJSON :: String -> [(String, JSValue)] -> JSValue
buildEventJSON typ event = showJSON $ makeObj [(jsonTypeField, showJSON typ), (jsonEventField, makeObj event)]

data EventDescriptor = EventDescriptor
    { eventType :: String
    , event :: JSValue
    } deriving (Show)

(!) :: JSON a => JSObject JSValue -> String -> Result a
(!) = flip valFromObj

instance JSON EventDescriptor where
    showJSON = undefined
    readJSON (JSObject obj) = 
        let event = fmap (map toEnum) (obj ! jsonEventField :: Result [Int]) :: Result [Char]
        in case event of
        (Ok event') -> EventDescriptor   <$>
                       obj ! jsonTypeField <*>
                       (decode (event' :: [Char]))
        (Error err) -> error err
    readJSON _ = mzero

attackEvent :: EventDescriptor -> Event
attackEvent (EventDescriptor typ event)
    | typ == "attack" =
        case readJSON event of
            (Ok (JSObject obj)) -> let (Ok char1) = obj ! "Char1"
                                       (Ok char2) = obj ! "Char2" 
                                   in AttackEvent (char1,char2)
    | typ == "characterMoved" =
        case readJSON event of
            (Ok (JSObject obj)) -> let (Ok goid) = obj ! "CharID"
                                       (Ok loc)  = obj ! "NewLocation"
                                   in CharacterMovedEvent goid loc
    | typ == "approveCharacterCreationRequest" = do
        case readJSON event of
            (Ok (JSObject obj)) -> let (Ok goid) = obj ! "ID"
                               in ApproveCharacterCreationRequestEvent goid

getEvent :: Socket -> IO ()
getEvent sock = do
    msg <- recv sock 1024
    putStrLn $ "received: " ++ C.unpack msg
    case decode (C.unpack msg) :: Result EventDescriptor of
        {-(Ok res) -> putStrLn $ show . fromJSObject $ res-}
        (Ok evt)  -> do
            dispatch evt
        (Error err) -> do
            putStrLn $ "ERROR: " ++ err

dispatch :: EventDescriptor -> IO ()
dispatch evt = do
    putStrLn . show $ attackEvent evt

sendEvent :: Socket -> Event -> IO ()
sendEvent sock evt = do
    let msg = C.pack (encode evt)
    putStrLn $ "sending: " ++ C.unpack msg
    sendAll sock msg
