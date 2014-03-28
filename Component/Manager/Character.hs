module Component.Manager.Character
( AttackType(..)
, attackObject
, attackComponent
, isCharacter
, getCharacter
, updateFromEvents
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
                  deriving (Show, Read, Eq, Ord)

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
        evts <- getEventsFromInstance ["attack", "death"]
        updateFromEvents evts
        return Nothing

updateFromEvents :: [Event] -> Instance ()
updateFromEvents [] = return ()
updateFromEvents (evt:evts) = do
    case evt of
        (DeathEvent _ goid) -> do
            s <- get
            let (CharacterManager ids) = characterManager s
            put $ s { characterManager = CharacterManager $ Map.delete goid ids }

            return ()
        otherwise -> return ()

    updateFromEvents evts

attackObject :: GOiD -> GOiD -> HitLocation -> Instance AttackType
attackObject id1 id2 hitLoc = do
    s <- get
    let cm@(CharacterManager ids) = characterManager s
        char1 = Map.lookup id1 ids
        char2 = Map.lookup id2 ids
    if isNothing char1 || isNothing char2
    then return Miss
    else let (Just justChar1) = char1
             (Just justChar2) = char2
             (hitMiss, char2', newGen) = attackComponent justChar1 justChar2 hitLoc (randomNumGen s)
             (Just rep1) = lookup (faction justChar1) (rep justChar1)
             reputationDiff = if faction justChar2 == faction justChar2
                                then -1 
                                else  1
             ids' = Map.update (const $ Just char2' { rep = replace (faction justChar1, rep1 + reputationDiff) (rep justChar1) []
                                                    }) id2 ids
         in do
             if health char2' <= 0
             then void $ pushEvent (DeathEvent id1 id2)
             else return ()

             s' <- get
             put $ s' { characterManager = CharacterManager ids'
                      , randomNumGen = newGen }
             return hitMiss


attackComponent :: CharacterComponent -> CharacterComponent -> HitLocation -> StdGen -> (AttackType, CharacterComponent, StdGen)
attackComponent char1 char2 hitLoc rnd =
    let (rndNum, newGen) = randomR (1, 100) rnd :: (Int, StdGen)
        damageDealt1 = case hitLoc of
            Head  -> if rndNum > 10 then 0 else damage char1 * 2.0
            Torso -> if rndNum > 90 then 0 else damage char1
            Legs  -> if rndNum > 70 then 0 else damage char1 * 1.5
        hitMiss = if damageDealt1 > 0
                  then Hit damageDealt1
                  else Miss
    in if health char2 <= 0 || health char1 <= 0
         then (Miss, char2, newGen)
         else (hitMiss, char2 { health = health char2 - damageDealt1 
                              }, newGen)

replace :: Reputation -> [Reputation] -> [Reputation] -> [Reputation]
replace _ [] rs = rs
replace f@(fac, _) (f'@(fac', rep):fx) rs = if fac' == fac
                                            then f:fx
                                            else replace f fx (rs ++ [f'])

isCharacter :: CharacterManager -> GOiD -> Bool
isCharacter (CharacterManager ids) = flip Map.member ids

getCharacter :: CharacterManager -> GOiD -> Maybe CharacterComponent
getCharacter (CharacterManager ids) = flip Map.lookup ids
