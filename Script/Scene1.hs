module Script.Scene1
( Scene1(..)
) where

import Control.Monad.Trans.State (state, runState)
import qualified Numeric.Matrix as Mat

import Script
import Component
import Component.Manager.Transform
import Component.Manager.Character
import Instance
import Math     (buildTranslationMatrix)

data Scene1 = Scene1
instance Script Scene1 where
    run (Scene1) = [ do
            putLine "Hey. Hey you see that guy over there?"
            putLine "He's probably up to something. Let's check it out."
            putLine "Don't just stand there."
            putLine "Use the `m` command to move around."

            return . state $ \s -> flip runState s $ do
                createObject $ buildObjectJSON (TransformComponent Blocked (Mat.unit 4 `Mat.times` buildTranslationMatrix (4,4) [5,0,0])) (CharacterComponent 10 5 10 Betuol [(Betuol, 0)]) Passive
                createObject $ buildObjectJSON (TransformComponent Blocked (Mat.unit 4 `Mat.times` buildTranslationMatrix (4,4) [5,0,0])) (CharacterComponent 10 5 10 Dunteg [(Dunteg, 0)]) Passive
                return ()

        , do
            putLine "Did you see him?"
            return . state $ \s -> ((), s)
        ]
