module Component.Manager.Character
( CharacterManager(..)
, AiFunction
, attackComponent
) where

import qualified Data.Map as Map
import Control.Monad.Trans.State
import Text.Show.Functions

import Component

type AiFunction = GOiD -> CharacterManager -> CharacterManager -> State CharacterManager ()
data CharacterComponent = CharacterComponent
    { health :: Float
    , damage :: Float
    , mana   :: Float
    } deriving Show

data CharacterManager = CharacterManager (Map.Map GOiD (CharacterComponent, AiFunction))
                        deriving Show

instance ComponentCreator CharacterManager where
	createComponent goid (CharacterManager ids) = Right . CharacterManager $ Map.insert goid (CharacterComponent 10 1 10, \gid m1 m2 -> state $ \s -> ((), s)) ids
	update = Right

attackComponent :: CharacterManager -> GOiD -> GOiD -> CharacterManager
attackComponent (CharacterManager ids) id1 id2 =
    let (Just char1@(CharacterComponent health1 damage1 _, _)) = Map.lookup id1 ids
        (Just char2@(CharacterComponent health2 damage2 mana2, aif2)) = Map.lookup id2 ids
    in CharacterManager $ Map.update (\x -> Just (CharacterComponent (health2 - damage1) damage2 mana2, aif2)) id2 ids
