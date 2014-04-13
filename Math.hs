module Math
where

import qualified Data.Packed.Matrix as Mat
import qualified Data.Packed.Vector as Vec

buildTranslationMatrix :: (Num e, Mat.Element e)
                       => Vec.Vector e
                       -> Mat.Matrix e
buildTranslationMatrix = Mat.buildMatrix 4 4 . buildTranslation

buildTranslation :: (Num e, Mat.Element e) => Vec.Vector e -> (Int, Int) -> e
buildTranslation vec (0,3) = vec Vec.@> 0
buildTranslation vec (1,3) = vec Vec.@> 1
buildTranslation vec (2,3) = vec Vec.@> 2
buildTranslation _   (0,0) = 1
buildTranslation _   (1,1) = 1
buildTranslation _   (2,2) = 1
buildTranslation _   (3,3) = 1
buildTranslation _   _     = 0
