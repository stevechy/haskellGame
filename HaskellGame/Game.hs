module HaskellGame.Game where

import HaskellGame.Types

import Data.IntMap.Lazy
import qualified Data.List

insertComponent :: GameEntityIdentifier -> GameComponent -> GameState -> GameState

insertComponent identifier gameComponent gameState =
    case gameComponent of
        PositionComponent p -> gameState { worldState = insert identifier p $ worldState gameState}
        CollisionComponent c -> gameState { boundingBoxState = insert identifier c $ boundingBoxState gameState}
        PhysicsComponent p -> gameState { physicsState = insert identifier p $ physicsState gameState}
        RenderingComponent r -> gameState { renderingHandlers = insert identifier r $ renderingHandlers gameState}

insertEntity  :: GameEntityIdentifier -> [GameComponent] -> GameState -> GameState
insertEntity identifier gameComponents gameState = Data.List.foldl' (\ gs gc -> insertComponent identifier gc gs) gameState gameComponents

