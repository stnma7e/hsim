module Component.Manager.Character
( CharacterManager(..)
, Faction(..)
, AiFunction
, attackComponent
, buildCharacterComponentJSON
) where

import qualified Data.Map as Map
import Control.Monad.Trans.State
import Text.Show.Functions
import Text.JSON
import Control.Monad

import Component
import Common

type AiFunction = GOiD -> CharacterManager -> CharacterManager
data CharacterComponent = CharacterComponent
    { health  :: Float
    , damage  :: Float
    , mana    :: Float
    , faction :: Faction
    } deriving Show

instance JSON CharacterComponent where
    showJSON = undefined
    readJSON (JSObject obj) = do
        health  <- obj ! "Health"  :: Result Float
        damage  <- obj ! "Damage"  :: Result Float
        mana    <- obj ! "Mana"    :: Result Float
        faction <- obj ! "Faction" :: Result String
        return $ CharacterComponent health damage mana (read faction)
    readJSON _ = mzero

data Faction = Betuol
             | Dunteg
             | Blitztal
               deriving ( Show
                        , Read
                        )

data CharacterManager = CharacterManager (Map.Map GOiD (CharacterComponent, AiFunction))
                        deriving Show

instance ComponentCreator CharacterManager where
    createComponent goid objData (CharacterManager ids) = 
        let cc = readJSON objData :: Result CharacterComponent
        in case cc of
            (Ok cc')-> Right . CharacterManager $ Map.insert goid (cc', \gid m1 -> m1) ids
            (Error err) -> error $ "creating character component " ++ err
    update = Right

attackComponent :: CharacterManager -> GOiD -> GOiD -> CharacterManager
attackComponent (CharacterManager ids) id1 id2 =
    let (Just char1@(CharacterComponent health1 damage1 _ _, _)) = Map.lookup id1 ids
        (Just char2@(CharacterComponent health2 damage2 mana2 faction2, aif2)) = Map.lookup id2 ids
    in CharacterManager $ Map.update (\x -> Just (CharacterComponent (health2 - damage1) damage2 mana2 faction2, aif2)) id2 ids

buildCharacterComponentJSON :: Float -> Float -> Float -> Faction -> String
buildCharacterComponentJSON health damage mana faction = "{"
                           ++ "\"Health\": "    ++ show health  ++ ","
                           ++ "\"Damage\": "    ++ show damage  ++ ","
                           ++ "\"Mana\": "      ++ show mana    ++ ","
                           ++ "\"Faction\": \"" ++ show faction ++ "\""
                           ++ "}"
