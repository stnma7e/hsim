module Script.Scene1
( Scene1(..)
) where

import Control.Monad.Trans.State (state, runState)
import qualified Numeric.Matrix as Mat

import Script
import Component.Manager.Transform
import Instance
import Math     (buildTranslationMatrix)

data Scene1 = Scene1
instance Script Scene1 where
    run (Scene1) = [do
            putLine "Hey. Hey you see that guy over there?"
            putLine "He's probably up to something. Let's check it out."
            putLine "Don't just stand there."
            putLine "Use the `m` command to move around."

            return . state $ \s -> flip runState s $ do
                createObject $ buildMatString Blocked (Mat.unit 4 `Mat.times` buildTranslationMatrix (4,4) [0,0,5])
                return ()

        , do
            putLine "Did you see him?"
            return . state $ \s -> ((), s)
        ] ++ repeat (return . state $ \s -> ((), s))
