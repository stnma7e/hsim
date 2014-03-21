module Net
( recvEvent
, sendEvent
) where

import System.IO
import Text.JSON

import Network.Socket hiding      (recv, sendAll)
import Network.Socket.ByteString  (recv, sendAll)
import Data.ByteString.Char8 as C (unpack, pack)

import Event
import Common

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
