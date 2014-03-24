module Component.Manager.Character
( attackObject
) where

import Control.Monad.Trans.State (state, get)
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

attackObject :: GOiD -> GOiD -> HitLocation -> Instance AttackType
attackObject id1 id2 hitLoc = state $ \s ->
    let (rnd, newGen) = randomR (1, 100) $ randomNumGen s :: (Int, StdGen)
        cm@(CharacterManager ids) = characterManager s
        (Just char1) = Map.lookup id1 ids
        damage1 = case hitLoc of
            Head  -> if rnd > 10 then 0 else 15
            Torso -> if rnd > 90 then 0 else 7
            Legs  -> if rnd > 70 then 0 else 10
        hitMiss = if damage1 > 0
                  then Hit damage1
                  else Miss
        (Just char2) = Map.lookup id2 ids
        (Just rep1) = lookup (faction char1) (rep char1)
        reputationDiff = if faction char1 == faction char2
                         then -1 
                         else  1
    in if health char2 <= 0 || health char1 <= 0
       then (Miss, s)
       else let ids' = Map.update (\x -> Just char2 { health = health char2 - damage1 }) id2 ids
            in (hitMiss, s { characterManager = CharacterManager $ Map.update (\x -> Just char2 { rep = replace (faction char1, rep1 + reputationDiff) [] (rep char1) }) id1 ids'
                      , randomNumGen = newGen })
                where replace :: Reputation -> [Reputation] -> [Reputation] -> [Reputation]
                      replace _ rs [] = rs
                      replace f@(fac, _) rs (f'@(fac', rep):fx) = if fac' == fac
                                                  then f:fx
                                                  else replace f (rs ++ [f']) fx
