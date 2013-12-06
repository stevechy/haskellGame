module HaskellGame.Physics.Simulator where

import HaskellGame.Types
import Data.IntMap.Lazy

simulateGameStatePhysics gameState = gameState { physicsState = Data.IntMap.Lazy.mapWithKey (simulateActorAdjustPhysics actorStatesItem) physicsStateItem  }           
  where actorStatesItem = actorStates gameState
        physicsStateItem = physicsState gameState
  
simulateActorAdjustPhysics :: ActorStates -> Int -> VelocityAcceleration -> VelocityAcceleration
simulateActorAdjustPhysics actorStates actorId actorPosition =
  case (Data.IntMap.Lazy.lookup actorId actorStates, actorPosition) of
              ((Just MovingRight), phys) -> phys {vx = 0.08}
              ((Just MovingLeft), phys) ->  phys {vx = -0.08}             
              ((Just Jumping), phys) -> phys {vy = -0.2}
              ((Just Idle), phys) -> phys {vx = 0}
              (_, phys) -> phys


applyPhysics :: Int -> GameState -> GameState              
applyPhysics delta gameState = gameState { worldState = Data.IntMap.Lazy.mapWithKey (simulatePhysics delta physicsStateItem) worldStateItem, physicsState = physicsStateItem  }
  where physicsStateItem = applyAcceleration delta $ physicsState gameState
        worldStateItem = worldState gameState
        
applyAcceleration :: Int-> PhysicsState -> PhysicsState
applyAcceleration delta physicsStateItem = Data.IntMap.Lazy.map (\ velocityAcceleration -> velocityAcceleration { vy = (vy velocityAcceleration ) + ((fromIntegral delta) * (ay velocityAcceleration))}) physicsStateItem
        
simulatePhysics :: Int -> PhysicsState -> Int -> Position -> Position
simulatePhysics delta physicsData id position = 
  case (Data.IntMap.Lazy.lookup id physicsData, position) of
       ((Just (VelocityAcceleration {vx = xvel, vy = yvel, ax = xaccel, ay = yaccel})), Position x y) -> Position (x + truncate (xvel * fromIntegral delta)) (y+ truncate (yvel * fromIntegral delta))
       (state, position) -> position