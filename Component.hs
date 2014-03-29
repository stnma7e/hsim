module Component
where

import Control.Monad.Trans.State
import Text.JSON
import Text.Show.Functions
import Control.Monad
import qualified Data.Map as Map
import qualified Numeric.Matrix as Mat
import System.Random
import Data.Maybe

import Common

eventTypeAttack                   = "attack"
eventTypeDeath                    = "death"
eventTypeKill                     = "kill"
eventTypeCharacterMoved           = "characterMoved"
eventTypeRequestCharacterCreation = "requestCharacterCreation"

type GOiD = Int

class ComponentCreator a where
	createComponent :: GOiD -> JSValue -> a -> Either String a
	update :: a -> Instance (Maybe String)

data Event = AttackEvent (GOiD, GOiD)
           | DeathEvent GOiD
           | KillEvent GOiD GOiD
           | CharacterMovedEvent GOiD [Float]
           | RequestCharacterCreationEvent String [Float]
             deriving (Show, Read, Eq)

data GameObjectJSON = GameObjectJSON
    { transform :: JSValue
    , character :: JSValue
    , ai        :: JSValue
    }
instance JSON GameObjectJSON where
    showJSON = undefined
    readJSON (JSObject obj) = do
        tm <- obj ! "Transform" :: Result JSValue
        cm <- obj ! "Character" :: Result JSValue
        am <- obj ! "Ai"        :: Result JSValue
        return $ GameObjectJSON tm cm am
    readJSON _ = mzero

getEventsFromInstance :: [String] -> Instance [Event]
getEventsFromInstance eventsToLookFor = do
        s <- get
        -- lets get a list of all the events we're going to look at
        let evts = map (`Map.lookup` (fst $ getEvents s)) eventsToLookFor
        -- then filter out all of the either empty lists or nonexistent event types (a.k.a values constructed with Nothing)
        return . join $ filter (not . null) [fromJust x | x <- evts, isJust x]

buildObjectJSON :: (JSON a, JSON b, JSON c) => a -> b -> c -> JSValue
buildObjectJSON tm cm am = showJSON $ makeObj [ ("Transform", showJSON tm)
                                              , ("Character", showJSON cm)
                                              , ("Ai", showJSON am)
                                              ]

pushEvent :: Event -> Instance ()
pushEvent evt = insertEvent evt $ case evt of
        (AttackEvent _)  -> eventTypeAttack
        (DeathEvent _)   -> eventTypeDeath
        (KillEvent _ _)    -> eventTypeKill
    where insertEvent :: Event -> String -> Instance ()
          insertEvent evt typ = state $ \s ->
              let (currentFrameEvents, nextFrameEvents) = getEvents s
                  eventsOfCurrentType = Map.lookup typ nextFrameEvents
                  newEventList = case eventsOfCurrentType of
                      (Just curEvts) -> Map.insert typ (evt : curEvts) nextFrameEvents
                      otherwise      -> Map.insert typ [evt] nextFrameEvents
              in ((), s { getEvents = (currentFrameEvents, newEventList) })

type EventList = Map.Map String [Event]

type Instance = State InstanceState
data InstanceState = InstanceState
    { player           :: GOiD
    , transformManager :: TransformManager
    , characterManager :: CharacterManager
    , aiManager        :: AiManager
    , getEvents        :: (EventList, EventList)
    , availiableIDS    :: [GOiD]
    , randomNumGen     :: StdGen
    } deriving Show

-------------------------------------
-- Managers --
-------------------------------------

--
-- Transform
--

data ObjectType = Blocked | Open
                  deriving (Show , Read, Eq)
data TransformComponent = TransformComponent
    { objType   :: ObjectType
    , getMatrix :: Mat.Matrix Float
    } deriving (Show, Eq)
type ComponentMap = Map.Map GOiD TransformComponent
type Grid = Map.Map (Int, Int) [GOiD]
data TransformManager = TransformManager
    { components       :: ComponentMap
    , spatialPartition :: Grid
    } deriving Show

--
-- Character
--

data HitLocation = Head | Torso | Legs
                   deriving (Show, Read)
data Faction = Betuol | Dunteg | Blitzal
               deriving (Show , Read, Eq, Ord)
data SpellType = Melee | Fire | Earth | Frost | Air
                 deriving (Show, Read, Eq)
data DamageType = DamageType Float SpellType
                 deriving (Show, Read, Eq)
type Reputation = (Faction, Int)
data CharacterEquipment = EmptyEquipment
                        | CharacterEquipment
    { weapon :: DamageType
    } deriving (Show, Read, Eq)
data CharacterComponent = CharacterComponent
    { health  :: Int
    , mana    :: Int
    , faction :: Faction
    , rep     :: [Reputation]
    , equipment :: CharacterEquipment
    } deriving (Show, Eq)
newtype CharacterManager = CharacterManager (Map.Map GOiD CharacterComponent)
                           deriving Show
damage :: CharacterComponent -> Float
damage char = case equipment char of
    (CharacterEquipment ce) -> let (DamageType damage _) = ce
                               in damage
    otherwise -> 0
--
-- AI
--

data AiComponent = Enemy | Passive
                   deriving (Show, Read)
type AiComputer = GOiD -> Instance ()
newtype AiManager = AiManager (Map.Map GOiD AiComputer)
                    deriving Show
