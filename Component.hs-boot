module Component
where

import System.Random
import Data.Map

import Common

type EventList = Map String [Event]

data Event = AttackEvent (GOiD, GOiD) Int
           | DeathEvent GOiD
           | KillEvent GOiD GOiD
           | CharacterMovedEvent GOiD [Float]
           | RequestCharacterCreationEvent String [Float]

data InstanceState a b c = InstanceState
    { player           :: GOiD
    , transformManager :: a
    , characterManager :: b
    , aiManager        :: c
    , getEvents        :: (EventList, EventList)
    , availiableIDS    :: [GOiD]
    , randomNumGen     :: StdGen
    }
