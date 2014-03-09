module Game
( Scene(..)
, Scene1(..)
) where

import Control.Monad.Trans.State (state, runState)

import Instance

class Scene a where
    -- takes a scene and a function to call to get input and update the world
    run :: a -> InstanceState -> IO (Instance ())

data Scene1 = Scene1
instance Scene Scene1 where
    run (Scene1) is = do
        putStrLn "Hey. Hey you see that guy over there?"
        putStrLn "He's probably up to something. Let's check it out."
        let (id, is') = runState createObject is
        return . state $ \s -> ((), is')
