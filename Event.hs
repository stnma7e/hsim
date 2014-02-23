module Event where

import System.IO
import Network.Socket hiding (recv, sendAll)
import Network.Socket.ByteString (recv, sendAll)
import Data.ByteString.Char8 as B (unpack, pack)
import Control.Monad
import Text.JSON
import Text.JSON.String
import qualified Numeric.Matrix as Mat
import Data.List

import Instance

data Event = AttackEvent (GOiD, GOiD)
           | CharacterMovedEvent GOiD [Float]
           | RequestCharacterCreationEvent String [Float]
           | ApproveCharacterCreationRequestdEvent GOiD
           | CharacterCreatedEvent GOiD
             deriving (Show)

instance JSON Event where
    readJSON = undefined
    showJSON (AttackEvent (char1, char2)) = buildEventJSON "attack" ((show char1)++" "++(show char2))
    showJSON (CharacterMovedEvent char loc) = buildEventJSON "characterMoved" ((show char)++" "++(show loc))
    showJSON (RequestCharacterCreationEvent charType loc) = buildEventJSON "requestCharacterCreation" (charType++" "++(show loc))

buildEventJSON :: String -> String -> JSValue
buildEventJSON typ event = showJSON $ toJSObject [("eventType", typ), ("event", "{"++ event ++"}")]

data EventDescriptor = EventDescriptor
    { eventType :: String
    , event :: String
    } deriving (Show)

instance JSON EventDescriptor where
    showJSON = undefined
    readJSON obj@(JSObject _) = do
        obj' <- (readJSON obj :: Result (JSObject String))
        let objList = fromJSObject obj'
        let eventType = (snd . head $ objList)
        let event = (snd . head . tail $ objList)
        return $ EventDescriptor eventType [x | x <- event, x `notElem` ['{','}']]
    readJSON _ = mzero

attackEvent :: EventDescriptor -> Event
attackEvent (EventDescriptor typ event)
    | typ == "attack" =
        let charList =  map read . words $ event
        in AttackEvent ((head charList), (head $ tail charList))
    | typ == "characterMoved" =
        let goid = read . head . words $ event
            -- characterMoved event takes the form of "<character GOiD> [x y z]"
            loc  = map read . words $ filter (`notElem` ['[',']']) (unwords . tail . words $ event)
        in CharacterMovedEvent goid loc

getEvent :: Socket -> IO Event
getEvent sock = do
    msg <- recv sock 1024
    putStrLn $ B.unpack msg
    case decode (B.unpack msg) :: Result EventDescriptor of
        {-(Ok res) -> putStrLn $ show . fromJSObject $ res-}
        (Ok evt)  -> do
            dispatch evt
            return $ attackEvent evt
        otherwise -> error "failed in parsing the JSON values"

dispatch :: EventDescriptor -> IO ()
dispatch evt@(EventDescriptor "attack" event) = do
    putStrLn . show $ attackEvent evt
dispatch evt = do
    putStrLn . show $ evt

sendEvent :: Socket -> Event -> IO ()
sendEvent sock evt = do
    let msg = B.pack (encode evt)
    putStrLn $ B.unpack msg
    sendAll sock msg
