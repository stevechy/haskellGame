module HaskellGame.Gameplay.Simulator where

import HaskellGame.Types
import Data.List
import Data.Maybe

import Data.IntMap.Lazy

processGameState events playerId gameState = 
  Data.List.foldr processEvent gameState events 
    where processEvent event gameState = 
            gameState { actorStates = Data.IntMap.Lazy.adjust (processActorEvent event) playerId (actorStates gameState) }
            
processActorEvent event actorState =                          
  case (event, actorState) of
    (MoveRight, _) -> MovingRight
    (MoveLeft, _) -> MovingLeft
    (Jump, _) -> Jumping
    (StopJump, _ ) -> Idle
    (CancelRight, _) -> Idle  
    (CancelLeft, _) -> Idle
        


simulateGameState gameState = gameState { worldState = Data.IntMap.Lazy.mapWithKey (simulateActor actorStatesItem) worldStateItem  }           
  where actorStatesItem = actorStates gameState
        worldStateItem = worldState gameState
  
simulateActor :: ActorStates -> Int -> Position -> Position
simulateActor actorStates actorId actorPosition =
  case (Data.IntMap.Lazy.lookup actorId actorStates, actorPosition) of
              ((Just MovingRight), (Position x y)) -> Position (x+5) y
              ((Just MovingLeft), (Position x y)) ->  Position (x-5) y             
              ((Just Jumping), (Position x y)) -> Position x (y-5)
              (_, position) -> position
              
