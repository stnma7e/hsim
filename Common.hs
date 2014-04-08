module Common
where

import Text.JSON

type GOiD = Int

-- borrowed from http://stackoverflow.com/a/17844540/2977341 
(!) :: JSON a => JSObject JSValue -> String -> Result a
(!) = flip valFromObj
