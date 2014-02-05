module HaskellGame.Gameplay.Simulator where

import HaskellGame.Types
import Data.List
import Data.Maybe

import Data.IntMap.Lazy

processGameState :: (GameState, GameEventQueues) -> (GameState, GameEventQueues)
processGameState (gameState, events) = 
  Data.List.foldr processEvent (gameState,events) (gameActions events)
    where processEvent event (gameState, events)  = 
            (gameState { actorStates = Data.IntMap.Lazy.adjust (processActorEvent (gameEvent event)) (identifier event) (actorStates gameState) }, events)

processGameStateOutputEvents :: (GameState, GameEventQueues) -> (GameState, GameEventQueues)
processGameStateOutputEvents (gameState, events) = Data.List.foldr processActorOutputEvents (gameState,events) (gameActions events)                

processActorEvent event actorState =                          
  case (event, actorState) of
    (MoveRight, _) -> MovingRight
    (MoveLeft, _) -> MovingLeft
    (Jump, _) -> Jumping
    (StopJump, _ ) -> Idle
    (CancelRight, _) -> Idle  
    (CancelLeft, _) -> Idle

processActorOutputEvents :: GameEvent GameAction -> (GameState, GameEventQueues)  -> (GameState, GameEventQueues)
processActorOutputEvents gameEvent (gameState, events)  = (gameStateChange gameState, eventChange events)
    where (gameStateChange, eventChange) = actorChange (gameState, events) gameEvent

actorChange (gameState, events) GameEvent { identifier = ident, gameEvent = event } =
    case event of
        MoveRight -> (adjustActor ident MovingRight, (\events -> events {physicsActions =  GameEvent ident (Impulse {impulseVx=0.08, impulseVy=0.0}): (physicsActions events)}))
        MoveLeft -> (adjustActor ident MovingLeft,  (\events -> events {physicsActions =  GameEvent ident (Impulse {impulseVx= -0.08, impulseVy=0.0}): (physicsActions events)}))
        Jump -> (id, (\events -> events {physicsActions =  GameEvent ident (Impulse {impulseVx= 0.0, impulseVy= -0.2}): (physicsActions events)}))
        StopJump -> (id, id)
        CancelRight -> (id, id) 
        CancelLeft -> (id, id)
      
adjustActor key actorState gameState = gameState { actorStates = Data.IntMap.Lazy.insert key actorState (actorStates gameState) }


              
