module Component.Manager.Character
( AttackType(..)
, SpellType(..)
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

data AttackType = Hit Int | Miss
                  deriving (Show, Read, Eq)

instance JSON CharacterComponent where
    showJSON char = showJSON $ makeObj [
          ("Health",     showJSON $ health           char)
        , ("Mana",       showJSON $ mana             char)
        , ("Faction",    showJSON . show $ faction   char)
        , ("Reputation", showJSON . show $ rep       char)
        , ("Equipment",  showJSON . show $ equipment char)
        ]
    readJSON (JSObject obj) = do
        health  <- obj ! "Health"     :: Result Int
        mana    <- obj ! "Mana"       :: Result Int
        faction <- obj ! "Faction"    :: Result String
        rep     <- obj ! "Reputation" :: Result String
        equip   <- obj ! "Equipment"  :: Result String
        return $ CharacterComponent health mana (read faction) (read rep) (read equip)
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

        (CharacterManager ids) <- liftM characterManager get
        let deadIds = Map.foldrWithKey (\goid comp acc -> if health comp <= 0 then goid:acc else acc) []  ids
        foldr (\is acc -> acc >>= const is) (return ()). map pushEvent $ map DeathEvent deadIds
        return Nothing

updateFromEvents :: [Event] -> Instance ()
updateFromEvents [] = return ()
updateFromEvents (evt:evts) = do
    case evt of
        (DeathEvent goid) -> do
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
    else let (Just char1') = char1
             (Just char2') = char2
             (hitMiss, health2', newGen) = attackComponent (health char1', health char2')
                                                           (DamageType (damage char1') Melee)
                                                           hitLoc
                                                           (randomNumGen s)
             (Just rep1) = lookup (faction char1') (rep char1')
             reputationDiff = if faction char2' == faction char2'
                                then -1 
                                else  1
             ids' = Map.update (const $ Just char2' { health = health2'
                                                    , rep = replace (faction char1', rep1 + reputationDiff) (rep char1') []
                                                    }) id2 ids
         in if health char2' <= 0
            then return Miss
            else do
                if health2' <= 0
                then pushEvent (DeathEvent id2) >>= \_ -> pushEvent (KillEvent id1 id2)
                else return ()

                s' <- get
                put $ s' { characterManager = CharacterManager ids'
                         , randomNumGen = newGen
                         }
                return hitMiss

replace :: Reputation -> [Reputation] -> [Reputation] -> [Reputation]
replace _ [] rs = rs
replace f@(fac, _) (f'@(fac', rep):fx) rs = if fac' == fac
                                            then f:fx
                                            else replace f fx (rs ++ [f'])

type Health = Int
attackComponent :: (Health, Health) -> DamageType -> HitLocation -> StdGen -> (AttackType, Health, StdGen)
attackComponent (health1, health2) (DamageType damage1 _) hitLoc rnd =
    let (rndNum, newGen) = randomR (1, 100) rnd :: (Int, StdGen)
        damageDealt1 = truncate $ case hitLoc of
            Head  -> if rndNum > 10 then 0 else damage1 * 2
            Torso -> if rndNum > 90 then 0 else damage1
            Legs  -> if rndNum > 70 then 0 else damage1 * 1.5
        hitMiss = if damageDealt1 > 0
                  then Hit damageDealt1
                  else Miss
    in if health2 <= 0 || health1 <= 0
         then (Miss, health2, newGen)
         else (hitMiss, health2 - damageDealt1, newGen)

isCharacter :: CharacterManager -> GOiD -> Bool
isCharacter (CharacterManager ids) = flip Map.member ids

getCharacter :: CharacterManager -> GOiD -> Maybe CharacterComponent
getCharacter (CharacterManager ids) = flip Map.lookup ids
