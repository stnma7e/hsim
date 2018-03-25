module Component.Manager.Character
( CharacterManager(..)
, CharacterComponent(..)

, Faction(..)
, SpellType(..)
, DamageType(..)
, CharacterEquipment(..)
, Reputation

, attackObject
, HitLocation(..)
, AttackResult(..)

, getCharacter
, getCharDamage 
, isCharacter

-- Testing
, attackComponent
) where

import Control.Monad.Trans.State
import Text.JSON
import System.Random
import Data.Maybe
import Control.Monad
import qualified Data.Map as Map

import Component
import Common

data HitLocation = Head | Torso | Legs
                   deriving (Show, Read, Eq)

data Faction = Betuol | Dunteg | Blitzal
               deriving (Show , Read, Eq, Ord)

data SpellType = Melee | Fire | Earth | Frost | Air
                 deriving (Show, Read, Eq)

data DamageType = DamageType Float [SpellType]
                 deriving (Show, Read, Eq)

type Reputation = (Faction, Int)

data CharacterEquipment = EmptyEquipment
                        | CharacterEquipment
    { weapon :: DamageType
    } deriving (Show, Read, Eq)

data CharacterComponent = CharacterComponent
    { getCharHealth  :: Int
    , getCharMana    :: Int
    , getCharFaction :: Faction
    , getCharRep     :: [Reputation]
    , getCharEquipment :: CharacterEquipment
    } deriving (Show, Eq)

newtype CharacterManager = CharacterManager (Map.Map GOiD CharacterComponent)
                           deriving Show

getCharDamage :: CharacterComponent -> Float
getCharDamage char = case getCharEquipment char of
    (CharacterEquipment ce) -> let (DamageType damageOfAttack _) = ce
                               in damageOfAttack
    _ -> 0

instance JSON CharacterComponent where
    showJSON char = showJSON $ makeObj [
          ("Health",     showJSON $ getCharHealth           char)
        , ("Mana",       showJSON $ getCharMana             char)
        , ("Faction",    showJSON . show $ getCharFaction   char)
        , ("Reputation", showJSON . show $ getCharRep       char)
        , ("Equipment",  showJSON . show $ getCharEquipment char)
        ]
    readJSON (JSObject obj) = do
        charHealth  <- obj ! "Health"     :: Result Int
        charMana    <- obj ! "Mana"       :: Result Int
        charFaction <- obj ! "Faction"    :: Result String
        charRep     <- obj ! "Reputation" :: Result String
        charEquip   <- obj ! "Equipment"  :: Result String
        return $ CharacterComponent charHealth charMana (read charFaction) (read charRep) (read charEquip)
    readJSON _ = mzero

instance ComponentCreator CharacterManager where
    createComponent goid objData (CharacterManager ids) =
        let cc = readJSON objData :: Result CharacterComponent
        in case cc of
            (Ok cc')-> Right . CharacterManager $ Map.insert goid cc' ids
            (Error err) -> error $ "creating character component: " ++ err
    update _ = do
        evts <- getEventsFromInstance ["attack", "death"]
        updateCharacterManagerFromEvents evts

        (CharacterManager ids) <- liftM (getManager Character) get
        -- the deleted components from earlier events won't be counted again
        -- because we already did our event update for last frame
        let deadIds = Map.foldrWithKey (\goid comp acc -> if getCharHealth comp <= 0 then goid:acc else acc) []  ids
        foldr ((\is acc -> acc >>= const is) . pushEvent . DeathEvent) (return ()) deadIds
        return Nothing

updateCharacterManagerFromEvents :: [Event] -> Instance ()
updateCharacterManagerFromEvents [] = return ()
updateCharacterManagerFromEvents (evt:evts) = do
    case evt of
        (DeathEvent goid) -> do
            s <- get
            let (CharacterManager ids) = getManager Character s
                cm = CharacterManager $ Map.delete goid ids
            put $ putManager Character (ComponentManager cm) s

            return ()
        _ -> return ()

    updateCharacterManagerFromEvents evts

data AttackResult = Hit Int | Miss
                    deriving (Show, Read, Eq)

attackObject :: GOiD -> GOiD -> HitLocation -> Instance AttackResult
attackObject id1 id2 hitLoc = do
    s <- get
    let (CharacterManager ids) = getManager Character s
        char1 = Map.lookup id1 ids
        char2 = Map.lookup id2 ids
    if isNothing char1 || isNothing char2
    then return Miss
    else let (Just char1') = char1
             (Just char2') = char2
             (hitMiss, getCharHealth2', newGen) = attackComponent (getCharHealth char1', getCharHealth char2')
                                                           (DamageType (getCharDamage char1') [Melee])
                                                           hitLoc
                                                           (randomNumGen s)
             (Just rep1) = lookup (getCharFaction char1') (getCharRep char1')
             reputationDiff = if getCharFaction char2' == getCharFaction char2'
                                then -1
                                else  1
             ids' = Map.update (const $ Just char2' { getCharHealth = getCharHealth2'
                                                    , getCharRep    = replaceReputation (getCharFaction char1', rep1 + reputationDiff) (getCharRep char1') []
                                                    }) id2 ids
         in if getCharHealth char2' <= 0
            then return Miss
            else do when (getCharHealth2' <= 0) $
                        pushEvent (KillEvent id1 id2)

                    pushEvent (AttackEvent (id1, id2) (getCharHealth char2' - getCharHealth2'))
                    s' <- liftM (updateInstance ids') get
                    put $ s' { randomNumGen = newGen
                             }
                    return hitMiss
    where updateInstance :: Map.Map GOiD CharacterComponent
                         -> InstanceState
                         -> InstanceState
          updateInstance ids' = putManager Character
                              $ ComponentManager (CharacterManager ids')

replaceReputation :: Reputation -> [Reputation] -> [Reputation] -> [Reputation]
replaceReputation _ [] rs = rs
replaceReputation f@(fac, _) (f'@(fac', _):fx) rs = if fac' == fac
                                            then f:fx
                                            else replaceReputation f fx (rs ++ [f'])

type Health = Int
attackComponent :: (Health, Health) -> DamageType -> HitLocation -> StdGen -> (AttackResult, Health, StdGen)
attackComponent (health1, health2) (DamageType damage1 _) hitLoc rnd =
    let (rndNum, newGen) = randomR (1, 100) rnd :: (Int, StdGen)
        damageDealt1 = truncate $ case hitLoc of
            Head    -> if rndNum > 10 then 0 else damage1 * 2
            Torso   -> if rndNum > 90 then 0 else damage1
            Legs    -> if rndNum > 70 then 0 else damage1 * 1.5
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

