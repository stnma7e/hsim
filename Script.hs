module Script
( Script(..)
, putLine
, putDialouge
) where

import Instance

class Script a where
    -- takes a scene and returns a list of scene computations that can be executed atomically
    run :: a -> [IO (Instance ())]

putLine :: String -> IO ()
putLine s = putStrLn $ "\t" ++ s

putDialouge :: String -> IO ()
putDialouge s = putLine $ "-- " ++ show s
