module MainSpec (spec) where

import Test.Hspec
import System.Random
import Control.Monad.Trans.State
import qualified Numeric.Matrix as Mat
import qualified Data.Map as Map
import Data.List

import Instance
import Math
import Component
import Component.Manager.Character
import Component.Manager.Transform
import Component.Manager.Ai

randomGen :: StdGen
randomGen = mkStdGen 1

charComponent = CharacterComponent 10 5 10 Betuol [(Betuol, 0)]
deadCharComponent = charComponent { health = 0 }

startingInstance :: InstanceState
startingInstance = flip execState emptyInstanceState $ do
    s <- get
    put $ s { randomNumGen = mkStdGen 1 }

    playerId <- createObject $ buildObjectJSON (TransformComponent Open (Mat.unit 4)) (CharacterComponent 10 5 10 Betuol [(Betuol, 0)]) Enemy

    s <- get
    put $ s { player = playerId }
    return playerId

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    transformManagerSpec
    characterManagerSpec
    instanceSpec

transformManagerSpec = describe "TransformManager" $ do
    context "getGridXY" $ do
        it "returns correct (x, y) component location from matrix" $ do
            let xy = getGridXY (buildTranslationMatrix (4,4) [1,0,0])
            xy `shouldBe` (1, 0)

    context "checkBlocked" $ do
        it "returns Blocked when a blocked component is present" $ do
            let mat = buildTranslationMatrix (4,4) [3,92,4]
            let (obj, s) = flip runState startingInstance $ do
                createObject $ buildObjectJSON (TransformComponent Blocked mat) charComponent Passive
            let isBlocked = obj `elem` checkCollision (getGridXY mat) (transformManager s)
            isBlocked `shouldBe` True

    context "getObjectLoc" $ do
        it "returns correct grid (X, Y) for a component" $ do
            let xy = getObjectLoc (player startingInstance) (transformManager startingInstance)
            xy `shouldBe` (0, 0)

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


characterManagerSpec = describe "CharacterManager" $ do
    context "attackComponent" $ do
        it "returns Miss when character has 0 health" $ do
            let (hitmiss, _, _) = attackComponent charComponent charComponent Torso randomGen
            hitmiss `shouldBe` Miss

    context "attackObject" $ do
        it "returns Miss when the character being attacked does not exist" $ do
            let (hitmiss, _) = flip runState startingInstance $ attackObject 0 200 Torso
            hitmiss `shouldBe` Miss
        it "returns Miss when the character attacking another does not exist" $ do
            let (hitmiss, _) = flip runState startingInstance $ attackObject 123 200 Torso
            hitmiss `shouldBe` Miss


instanceSpec = describe "Instance" $ do
    context "pushEvent" $ do
        it "puts an event in next frame's event list" $ do
            let (Just nextFramesDeathEvents, s) = flip runState startingInstance $ do
                pushEvent $ DeathEvent 1 12
                s <- get
                return . Map.lookup "death" . snd $ getEvents s
            (DeathEvent 1 12 `elem` nextFramesDeathEvents) `shouldBe` True
