module Game
( Scene(..)
, Scene1(..)
) where

import Control.Monad.Trans.State (state, runState)
import qualified Numeric.Matrix as Mat

import Instance
import Math

class Scene a where
    -- takes a scene and a function to call to get input and update the world
    run :: a -> InstanceState -> IO (Instance ())

data Scene1 = Scene1
instance Scene Scene1 where
    run (Scene1) is = do
        putStrLn "Hey. Hey you see that guy over there?"
        putStrLn "He's probably up to something. Let's check it out."
        let (id, is') = flip runState is $ do
            goid <- createObject
            moveObject goid ((Mat.unit 4) `Mat.times` buildTranslationMatrix (4,4) [0,0,5])
        return . state $ \s -> ((), is')
