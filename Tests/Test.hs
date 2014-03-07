module Main

where

import Test.HUnit

import HaskellGame.Types
import HaskellGame.Game

import qualified HaskellGame.Physics.Simulator
import qualified HaskellGame.Physics.CollisionDetector

import qualified Data.IntMap.Lazy


tests :: Test
tests = TestList [   
    TestLabel "AccelTest"
        (TestCase 
            (do  let intialGameState = HaskellGame.Types.emptyGameState
                 let playerEntityId = 201404
                 let playerEntity = GameEntity playerEntityId [toComponent $ Position 5 5,
                                                         toComponent $ VelocityAcceleration {vx = 0, vy = 0.00, ax = 0, ay = 0.0002},
                                                         toComponent $ BoundingBox 0 0 66 92]
                 let gameState = insertEntity intialGameState playerEntity
                 let physicsTimeInterval = 1000
                 let (gameStateAfterPhysics, _) = HaskellGame.Physics.Simulator.applyPhysics physicsTimeInterval (gameState, HaskellGame.Types.emptyGameEventQueues)
                 let positionAfterPhysics = Data.IntMap.Lazy.lookup playerEntityId $ worldState gameStateAfterPhysics
                 assertEqual "Force was applied" (Just $ Position 5 205) positionAfterPhysics)
        ),
    TestLabel "Collision Test"
        (TestCase 
            (do  let intialGameState = HaskellGame.Types.emptyGameState
                 let playerEntityId = 20140401
                 let playerEntity = GameEntity playerEntityId [toComponent $ Position 5 5,                                                         
                                                         toComponent $ BoundingBox 0 0 10 10]
                 let enemyEntityId = 20140402
                 let enemyEntity = GameEntity enemyEntityId [toComponent $ Position 5 5,                                                         
                                                         toComponent $ BoundingBox 0 0 5 5]
                 let gameState = insertEntities intialGameState [playerEntity, enemyEntity]
                 
                 let [((collisionEntityA,_,_), (collisionEntityB,_,_)) ] = HaskellGame.Physics.CollisionDetector.collisions gameState
                 assertEqual "Collisions detected" (playerEntityId, enemyEntityId) (collisionEntityA, collisionEntityB))
        )
    ]


main :: IO ()
main = do _ <- runTestTT tests
          return ()
