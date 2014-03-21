module Event.Attack
( AttackEvent(..)
) where

import Text.JSON

import Component
import Common
import Event

data AttackEvent = AttackEvent (GOiD, GOiD)
                   deriving Show

instance JSON AttackEvent where
    readJSON = undefined
    showJSON (AttackEvent (char1, char2)) =
        buildEventJSON "attack" [("Char1", showJSON char1), ("Char2", showJSON char2)]

instance Event AttackEvent where
    getEvent (EventDescriptor "attack" event) = 
        case readJSON event of
            (Ok (JSObject obj)) -> let (Ok char1) = obj ! "Char1"
                                       (Ok char2) = obj ! "Char2" 
                                   in AttackEvent (char1,char2)
