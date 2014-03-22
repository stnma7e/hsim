module Component
where

import Text.JSON
import Control.Monad

import Common

type GOiD = Int

class ComponentCreator a where
	createComponent :: GOiD -> JSValue -> a -> Either String a
	update :: a -> Either String a

data GameObjectJSON = GameObjectJSON
    { transform :: JSValue
    , character :: JSValue
    }

instance JSON GameObjectJSON where
    showJSON = undefined
    readJSON (JSObject obj) = 
        let tm = obj ! "Transform" :: Result JSValue
            cm = obj ! "Character" :: Result JSValue
        in case tm of
            (Ok tm') -> return $ case cm of
                (Ok cm')    -> GameObjectJSON tm' cm' 
                (Error err) -> error $ "unable to determine `Character` from JSON component " ++ err
            (Error err) -> error $ "unable to determine `Transform` from JSON component " ++ err
    readJSON _ = mzero
