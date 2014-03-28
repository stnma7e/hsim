module MainSpec (spec) where

import Test.Hspec
import System.Random
import Control.Monad.Trans.State
import Numeric.Matrix

import Instance
import Math
import Component
import Component.Manager.Character
import Component.Manager.Transform

randomGen :: StdGen
randomGen = mkStdGen 1

comp = CharacterComponent 00 5 10 Betuol [(Betuol, 0)] 

startingInstance :: InstanceState
startingInstance = flip execState emptyInstanceState $ start randomGen

main :: IO ()
main = hspec $ spec

spec :: Spec
spec = do
    characterManagerSpec
    transformManagerSpec

transformManagerSpec :: Spec
transformManagerSpec = do
    describe "transformManager" $ do
        context "getGridXY" $ do
            it "will return correct (x, y) component location from matrix" $ do
                let xy = getGridXY (buildTranslationMatrix (4,4) [1,0,0])
                xy `shouldBe` (1, 0)
        context "moveObject" $ do
            it "can increase x location by one" $ do
                let s = startingInstance
                let ((err, maybePlayerLocation), _) = flip runState s $ do
                    err <- moveObject (player s) (buildTranslationMatrix (4,4) [1,0,0])
                    s' <- get
                    let objs = getObjectsAt (1, 0) (transformManager s')
                    return (err, lookup (player s') objs)
                err `shouldBe` Nothing
                maybePlayerLocation `shouldBe` Just (TransformComponent Open (buildTranslationMatrix (4,4) [1,0,0]))

characterManagerSpec :: Spec
characterManagerSpec = do
    describe "characterManager" $ do
        context "attackComponent" $ do
            it "returns Miss when character has 0 health" $ do
                let (hitmiss, _, _) = attackComponent comp comp Torso randomGen
                hitmiss `shouldBe` Miss

        context "attackObject" $ do
            it "returns Miss when the character being attacked does not exist" $ do
                let (hitmiss, _) = flip runState startingInstance $ attackObject 0 200 Torso
                hitmiss `shouldBe` Miss
            it "returns Miss when the character attacking another does not exist" $ do
                let (hitmiss, _) = flip runState startingInstance $ attackObject 123 200 Torso
                hitmiss `shouldBe` Miss
