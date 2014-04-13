module Script.Scene1
( Scene1(..)
) where

import Control.Monad.Trans.State (state, runState)
import qualified Data.Packed.Vector as Vec

import Script
import Component
import Instance
import Math
import Component.Manager.Transform
import Component.Manager.Character
import Component.Manager.Ai

data Scene1 = Scene1
instance Script Scene1 where
    run (Scene1) =
        [ do putLine "* A man comes running down the street. *"
             putDialouge "The emperor has been killed. The emperor has been killed."
             putDialouge "Don't just stand there."
             putLine "Use the `m` command to move around."

             return . state $ \s -> flip runState s $ do
                 _ <- createObject $ buildObjectJSON (TransformComponent Blocked (buildTranslationMatrix (Vec.fromList [5,0,0,1])))
                                                     (CharacterComponent 10 10 Betuol [(Betuol, 0)] (CharacterEquipment $ DamageType 5 [Melee]))
                                                     Passive
                 _ <- createObject $ buildObjectJSON (TransformComponent Blocked (buildTranslationMatrix (Vec.fromList [5,0,0,1])))
                                                     (CharacterComponent 10 10 Dunteg [(Dunteg, 0)] (CharacterEquipment $ DamageType 5 [Melee]))
                                                     Follow
                 return ()

        , do putLine "Did you see him?"
             return . state $ \s -> ((), s)
        ]
