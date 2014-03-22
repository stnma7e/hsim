module Component.Manager.Character
( CharacterManager(..)
, CharacterComponent(..)
, Faction(..)
, AiFunction
, attackComponent
) where

import Control.Monad.Trans.State (state)
import qualified Data.Map as Map
import Text.JSON
import Control.Monad
import Data.List

import Component
import Common

instance JSON CharacterComponent where
    showJSON (CharacterComponent health damage mana faction rep) = showJSON $ makeObj [
          ("Health",     showJSON health)
        , ("Damage",     showJSON damage)
        , ("Mana",       showJSON mana)
        , ("Faction",    showJSON $ show faction)
        , ("Reputation", showJSON $ show rep)
        ]
    readJSON (JSObject obj) = do
        health  <- obj ! "Health"     :: Result Float
        damage  <- obj ! "Damage"     :: Result Float
        mana    <- obj ! "Mana"       :: Result Float
        faction <- obj ! "Faction"    :: Result String
        rep     <- obj ! "Reputation" :: Result String
        return $ CharacterComponent health damage mana (read faction) (read rep)
    readJSON _ = mzero

instance ComponentCreator CharacterManager where
    createComponent goid objData (CharacterManager ids) = 
        let cc = readJSON objData :: Result CharacterComponent
        in case cc of
            (Ok cc')-> Right . CharacterManager $ Map.insert goid (cc', \gid m1 -> m1) ids
            (Error err) -> error $ "creating character component " ++ err
    update _ = state $ \s -> (Nothing, s)

attackComponent :: CharacterManager -> GOiD -> GOiD -> CharacterManager
attackComponent cm@(CharacterManager ids) id1 id2 =
    let (Just (char1, aif1)) = Map.lookup id1 ids
        (Just (char2, aif2)) = Map.lookup id2 ids
        (Just rep1) = lookup (faction char1) (rep char1)
        reputationDiff = if (faction char1) == (faction char2)
                         then -1 
                         else  1
    in if (health char2) < 0 || (health char1) < 0
       then cm
       else let ids' = Map.update (\x -> Just (char2 { health = (health char2) - (damage char1) }, aif2)) id2 ids
            in CharacterManager $ Map.update (\x -> Just (char2 { rep = replace ((faction char1), rep1 + reputationDiff) [] (rep char1) }, aif1)) id1 ids'
                where replace :: Reputation -> [Reputation] -> [Reputation] -> [Reputation]
                      replace _ rs [] = rs
                      replace f@(fac, _) rs (f'@(fac', rep):fx) = if fac' == fac
                                                  then f:fx
                                                  else replace f (rs ++ [f']) fx
