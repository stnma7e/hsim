module Component.Manager.Character
( CharacterManager(..)
, AiFunction
) where

import qualified Data.Map as Map
import Control.Monad.Trans.State
import Text.Show.Functions

import Component

type AiFunction = GOiD -> CharacterManager -> CharacterManager -> State CharacterManager ()
type CharacterComponent = [Int]

data CharacterManager = CharacterManager (Map.Map GOiD (CharacterComponent, AiFunction))
                        deriving Show

instance ComponentCreator CharacterManager where
	createComponent goid (CharacterManager ids) = Right . CharacterManager $ Map.insert goid ([1,12,4,43,34,3], \gid m1 m2 -> state $ \s -> ((), s)) ids
	update m = Right m
