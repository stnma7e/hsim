module Event
( EventDescriptor(..)
, Event(..)
, buildEventJSON
) where

import Control.Monad
import Control.Applicative
import System.IO
import Text.JSON

import Network.Socket hiding      (recv, sendAll)
import Network.Socket.ByteString  (recv, sendAll)
import Data.ByteString.Char8 as C (unpack, pack)

import Instance
import Common

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
