module Component
where

type GOiD = Int

class ComponentCreator a where
	createComponent :: GOiD -> String -> a -> Either String a
	update :: a -> Either String a
