module Event
( EventDescriptor(..)
, Event(..)
, buildEventJSON
, (!)
, recvEvent
, sendEvent
) where

import Control.Monad
import Control.Applicative
import System.IO
import Text.JSON

import Network.Socket hiding      (recv, sendAll)
import Network.Socket.ByteString  (recv, sendAll)
import Data.ByteString.Char8 as C (unpack, pack)

import Instance

jsonTypeField  = "Type"
jsonEventField = "Event"

class (JSON a, Show a) => Event a where
    getEvent :: EventDescriptor -> a

buildEventJSON :: String -> [(String, JSValue)] -> JSValue
buildEventJSON typ event = showJSON $ makeObj [(jsonTypeField, showJSON typ), (jsonEventField, makeObj event)]

data EventDescriptor = EventDescriptor
    { eventType :: String
    , eventData :: JSValue
    } deriving (Show)

-- borrowed from http://stackoverflow.com/a/17844540/2977341 
(!) :: JSON a => JSObject JSValue -> String -> Result a
(!) = flip valFromObj

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

recvEvent :: Socket -> IO (Either String EventDescriptor)
recvEvent sock = do
    msg <- recv sock 1024
    putStrLn $ "received: " ++ C.unpack msg
    case decode (C.unpack msg) :: Result EventDescriptor of
        (Ok evt)    -> return $ Right evt
        (Error err) -> return $ Left ("ERROR: " ++ err)

sendEvent :: (Event a) => Socket -> a -> IO ()
sendEvent sock evt = do
    let msg = C.pack (encode evt)
    putStrLn $ "sending: " ++ C.unpack msg
    sendAll sock msg
