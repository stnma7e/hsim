module Math
where

import qualified Numeric.Matrix as Mat

buildTranslationMatrix :: Mat.MatrixElement e => (Int,Int) -> [e] -> Mat.Matrix e
buildTranslationMatrix dimensions vec = Mat.matrix dimensions (buildTranslation vec)

buildTranslation :: Mat.MatrixElement e => [e] -> (Int, Int) -> e
buildTranslation vec (1,4) = vec!!0
buildTranslation vec (2,4) = vec!!1
buildTranslation vec (3,4) = vec!!2
buildTranslation _   (1,1) = 1
buildTranslation _   (2,2) = 1
buildTranslation _   (3,3) = 1
buildTranslation _   (4,4) = 1
buildTranslation _   _     = 0
