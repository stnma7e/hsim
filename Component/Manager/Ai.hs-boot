module Component.Manager.Ai
where

import qualified Data.Map as Map

import Common
import {-# SOURCE #-} Component
import {-# SOURCE #-} Component.Manager.Transform
import {-# SOURCE #-} Component.Manager.Character

data AiComponent = Enemy | Passive | Follow | Guard

type AiComputer = GOiD -> InstanceState TransformManager CharacterManager AiManager -> AiManager

newtype AiManager = AiManager (Map.Map GOiD AiComputer)
