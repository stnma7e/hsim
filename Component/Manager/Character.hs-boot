module Component.Manager.Character
where

import Data.Map

import Common

data Faction = Betuol | Dunteg | Blitzal

data SpellType = Melee | Fire | Earth | Frost | Air

data DamageType = DamageType Float [SpellType]

type Reputation = (Faction, Int)

data CharacterEquipment = EmptyEquipment
                        | CharacterEquipment
    { weapon :: DamageType
    }

data CharacterComponent = CharacterComponent
    { health  :: Int
    , mana    :: Int
    , faction :: Faction
    , rep     :: [Reputation]
    , equipment :: CharacterEquipment
    }

newtype CharacterManager = CharacterManager (Map GOiD CharacterComponent)
