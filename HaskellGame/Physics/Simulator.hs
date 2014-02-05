module HaskellGame.Physics.Simulator where

import HaskellGame.Types
import Data.IntMap.Lazy
import qualified Data.List

applyPhysicsChanges :: (GameState, GameEventQueues) -> (GameState, GameEventQueues)
applyPhysicsChanges (gameState, eventQueues) = (gameState { physicsState = Data.IntMap.Lazy.mapWithKey (applyPhysicsChange physicsEvents) physicsStateItem }, eventQueues )
    where physicsEvents = physicsActions eventQueues
          physicsStateItem = physicsState gameState

applyPhysicsChange :: [GameEvent PhysicsAction] -> GameEntityIdentifier -> VelocityAcceleration -> VelocityAcceleration
applyPhysicsChange events ident velAccel = Data.List.foldr (\velAccel gameEvent -> applyPhysicsEvent ident gameEvent velAccel ) (velAccel)  events

applyPhysicsEvent :: GameEntityIdentifier -> VelocityAcceleration -> GameEvent PhysicsAction -> VelocityAcceleration
applyPhysicsEvent ident velAccel gameEv = if ident == (identifier gameEv) then velAccel { vx = (vx velAccel) + (impulseVx (gameEvent gameEv)), vy = (vy velAccel) + (impulseVy (gameEvent gameEv))} else velAccel

simulateGameStatePhysics :: (GameState, GameEventQueues) -> (GameState, GameEventQueues)
simulateGameStatePhysics (gameState, eventQueues) = (gameState { physicsState = Data.IntMap.Lazy.mapWithKey (simulateActorAdjustPhysics actorStatesItem) physicsStateItem  } ,eventQueues)           
  where actorStatesItem = actorStates gameState
        physicsStateItem = physicsState gameState
  
simulateActorAdjustPhysics :: ActorStates -> Int -> VelocityAcceleration -> VelocityAcceleration
simulateActorAdjustPhysics actorStateContainer actorId actorPosition =
  case (Data.IntMap.Lazy.lookup actorId actorStateContainer, actorPosition) of
              ((Just MovingRight), phys) -> phys {vx = 0.08}
              ((Just MovingLeft), phys) ->  phys {vx = -0.08}             
              ((Just Jumping), phys) -> phys {vy = -0.2}
              ((Just Idle), phys) -> phys {vx = 0}
              (_, phys) -> phys


applyPhysics :: Int -> (GameState, GameEventQueues) -> (GameState, GameEventQueues)              
applyPhysics delta (gameState, eventQueues) = (gameState { worldState = Data.IntMap.Lazy.mapWithKey (simulatePhysics delta physicsStateItem) worldStateItem, physicsState = physicsStateItem  }, eventQueues)
  where physicsStateItem = applyAcceleration delta $ physicsState gameState
        worldStateItem = worldState gameState
        
applyAcceleration :: Int-> PhysicsState -> PhysicsState
applyAcceleration delta physicsStateItem = Data.IntMap.Lazy.map (\ velocityAcceleration -> velocityAcceleration { vy = (vy velocityAcceleration ) + ((fromIntegral delta) * (ay velocityAcceleration))}) physicsStateItem
        
simulatePhysics :: Int -> PhysicsState -> Int -> Position -> Position
simulatePhysics delta physicsData identifier position = 
  case (Data.IntMap.Lazy.lookup identifier physicsData, position) of
       ((Just (VelocityAcceleration {vx = xvel, vy = yvel, ax = xaccel, ay = yaccel})), Position x y) -> Position (x + truncate (xvel * fromIntegral delta)) (y+ truncate (yvel * fromIntegral delta))
       (state, position) -> position