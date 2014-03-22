module Component
where

import Control.Monad.Trans.State
import Text.JSON
import Text.Show.Functions
import Control.Monad
import qualified Data.Map as Map
import qualified Numeric.Matrix as Mat

import Common

type GOiD = Int

class ComponentCreator a where
	createComponent :: GOiD -> JSValue -> a -> Either String a
	update :: a -> Instance (Maybe String)

data Event = AttackEvent (GOiD, GOiD)
           | CharacterMovedEvent GOiD [Float]
           | RequestCharacterCreationEvent String [Float]
             deriving Show

data GameObjectJSON = GameObjectJSON
    { transform :: JSValue
    , character :: JSValue
    }

instance JSON GameObjectJSON where
    showJSON = undefined
    readJSON (JSObject obj) = 
        let tm = obj ! "Transform" :: Result JSValue
            cm = obj ! "Character" :: Result JSValue
        in case tm of
            (Ok tm') -> return $ case cm of
                (Ok cm')    -> GameObjectJSON tm' cm' 
                (Error err) -> error $ "unable to determine `Character` from JSON component " ++ err
            (Error err) -> error $ "unable to determine `Transform` from JSON component " ++ err
    readJSON _ = mzero

type Instance = State InstanceState
data InstanceState = InstanceState
    { getPlayer           :: GOiD
    , getTransformManager :: TransformManager
    , getCharacterManager :: CharacterManager
    , getEvents           :: Map.Map String [Event]
    , availiableIDS       :: [GOiD]
    } deriving (Show)

-------------------------------------
-- Managers --
-------------------------------------

--
-- Transform
--

data ObjectType = Blocked | Open
                  deriving (Show , Read)
data TransformComponent = TransformComponent ObjectType (Mat.Matrix Float)
                          deriving Show
type ComponentMap = Map.Map GOiD TransformComponent
type Grid = Map.Map (Int, Int) [GOiD]
data TransformManager = TransformManager
    { components       :: ComponentMap
    , spatialPartition :: Grid
    } deriving  Show

--
-- Character
--

data Faction = Betuol
               deriving (Show , Read)
data CharacterComponent = CharacterComponent
    { health  :: Float
    , damage  :: Float
    , mana    :: Float
    , faction :: Faction
    } deriving Show
type AiFunction = GOiD -> CharacterManager -> CharacterManager
data CharacterManager = CharacterManager (Map.Map GOiD (CharacterComponent, AiFunction))
                        deriving Show
