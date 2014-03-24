module Component.Manager.Character
( attackObject
, isCharacter
, getCharacter
) where

import Control.Monad.Trans.State (state, get, put)
import qualified Data.Map as Map
import Text.JSON
import Control.Monad
import Control.Applicative
import Data.List
import Data.Maybe
import System.Random

import Component
import Common

data AttackType = Hit Float | Miss
                  deriving (Show, Read)

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
            (Ok cc')-> Right . CharacterManager $ Map.insert goid cc' ids
            (Error err) -> error $ "creating character component: " ++ err
    update _ = do
        evts <- getEventsFromInstance ["attack"]
        updateFromEvents evts
        return Nothing

updateFromEvents :: [Event] -> Instance ()
updateFromEvents [] = return ()
updateFromEvents (evt:evts) = do
    case evt of
        (AttackEvent (id1, id2)) -> attackObject id1 id2 Torso >>= \_ -> return ()
        otherwise -> return ()
    updateFromEvents evts

isCharacter :: CharacterManager -> GOiD -> Bool
isCharacter (CharacterManager ids) = flip Map.member ids

getCharacter :: CharacterManager -> GOiD -> Maybe CharacterComponent
getCharacter (CharacterManager ids) = flip Map.lookup ids

attackObject :: GOiD -> GOiD -> HitLocation -> Instance AttackType
attackObject id1 id2 hitLoc = do
    s <- get
    let (rnd, newGen) = randomR (1, 100) $ randomNumGen s :: (Int, StdGen)
        cm@(CharacterManager ids) = characterManager s
        (Just char1) = Map.lookup id1 ids
        damageDealt1 = case hitLoc of
            Head  -> if rnd > 10 then 0 else (damage char1 * 2.0)
            Torso -> if rnd > 90 then 0 else (damage char1)
            Legs  -> if rnd > 70 then 0 else (damage char1 * 1.5)
        hitMiss = if damageDealt1 > 0
                  then Hit damageDealt1
                  else Miss
        (Just char2) = Map.lookup id2 ids
        (Just rep1) = lookup (faction char1) (rep char1)
        reputationDiff = if faction char1 == faction char2
                         then -1 
                         else  1
    if health char2 <= 0 || health char1 <= 0
    then return Miss
    else do
        let newCharacterHealth = health char2 - damageDealt1
            ids' = Map.update (\x -> Just char2 { health = newCharacterHealth }) id2 ids

        if newCharacterHealth <= 0
        then do
            pushEvent $ DeathEvent id1 id2
            return ()
        else return ()

        s' <- get
        put $ s' { characterManager = CharacterManager $ Map.update (\x -> Just char2 { rep = replace (faction char1, rep1 + reputationDiff) (rep char1) [] } ) id1 ids'
                 , randomNumGen = newGen }
        return hitMiss

             where replace :: Reputation -> [Reputation] -> [Reputation] -> [Reputation]
                   replace _ [] rs = rs
                   replace f@(fac, _) (f'@(fac', rep):fx) rs = if fac' == fac
                                                               then f:fx
                                                               else replace f fx (rs ++ [f'])
