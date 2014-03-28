module MainSpec (spec) where

import Test.Hspec
import System.Random
import Control.Monad.Trans.State

import Instance
import Component
import Component.Manager.Character

randomGen :: StdGen
randomGen = mkStdGen 1

comp = CharacterComponent 00 5 10 Betuol [(Betuol, 0)] 

startingInstance :: InstanceState
startingInstance = flip execState emptyInstanceState $ start randomGen

main :: IO ()
main = hspec $ spec

spec :: Spec
spec = characterManagerSpec

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
