{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module HaskellGame.Game where

import HaskellGame.Types

import Data.IntMap.Lazy
import qualified Data.List



data GameComponent = PositionComponent Position 
                       | CollisionComponent BoundingBox 
                       | PhysicsComponent VelocityAcceleration 
                       | RenderingComponent RenderingHandler 
                       | ActorComponent ActorState
                       | AnimationComponent AnimationClip

class GameComponentStore a where
    toComponent :: a -> GameComponent

instance GameComponentStore Position where
    toComponent = PositionComponent 

instance GameComponentStore BoundingBox where
    toComponent = CollisionComponent 

instance GameComponentStore VelocityAcceleration where
    toComponent = PhysicsComponent 

instance GameComponentStore RenderingHandler where
    toComponent = RenderingComponent 

instance GameComponentStore ActorState where
    toComponent = ActorComponent 

instance GameComponentStore AnimationClip where
    toComponent = AnimationComponent 


data GameEntity = GameEntity GameEntityIdentifier [GameComponent]

insertComponent :: GameEntityIdentifier -> GameComponent -> GameState -> GameState

insertComponent identifier gameComponent gameState =
    case gameComponent of
        PositionComponent p -> gameState { worldState = insert identifier p $ worldState gameState}
        CollisionComponent c -> gameState { boundingBoxState = insert identifier c $ boundingBoxState gameState}
        PhysicsComponent p -> gameState { physicsState = insert identifier p $ physicsState gameState}
        RenderingComponent r -> gameState { renderingHandlers = insert identifier r $ renderingHandlers gameState}
        ActorComponent a -> gameState { actorStates = insert identifier a $ actorStates gameState}
        AnimationComponent a -> gameState { _animationStates = insert identifier a $ _animationStates gameState}

insertEntity  :: GameState -> GameEntity -> GameState
insertEntity gameState (GameEntity identifier gameComponents)  = Data.List.foldl' (\ gs gc -> insertComponent identifier gc gs) gameState gameComponents

insertEntities :: GameState -> [GameEntity] -> GameState
insertEntities gameState gameEntities = Data.List.foldl' (\ gs gameEntity -> insertEntity gs gameEntity) gameState gameEntities

