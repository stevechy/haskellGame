module HaskellGame.Types where

import Data.IntMap.Lazy
import qualified Data.Array
import qualified HaskQuery

import Graphics.UI.SDL.Types
import Graphics.UI.SDL.TTF

type GameEntityIdentifier = Int

data Position = Position { _x :: Int, _y :: Int} deriving (Show, Eq)

data Player = Player { _playerId:: Int, _playerObjectIdentifier :: Int}


data VelocityAcceleration = VelocityAcceleration { vx::Double, vy::Double, ax::Double, ay::Double}

data BoundingBox = BoundingBox { relX :: Int, relY :: Int, boxWidth :: Int, boxHeight::Int} deriving (Show, Eq)

type BoundingBoxState = Data.IntMap.Lazy.IntMap BoundingBox

type PhysicsState = Data.IntMap.Lazy.IntMap VelocityAcceleration

type WorldState = Data.IntMap.Lazy.IntMap Position

type RenderingHandler = GameEntityIdentifier -> GameState -> Surface -> IO ()

type RenderingHandlers = Data.IntMap.Lazy.IntMap RenderingHandler

data AnimationClip = AnimationClip { _resourceId :: GameEntityIdentifier , _startTime :: Int, _rate :: Int }

type AnimationStates = Data.IntMap.Lazy.IntMap AnimationClip

type GraphicResources = Data.IntMap.Lazy.IntMap GraphicResource

data GraphicResource = Image Graphics.UI.SDL.Types.Surface | ImageSet (Data.Array.Array Int Graphics.UI.SDL.Types.Surface)

type CollisionUnit = (GameEntityIdentifier, BoundingBox, Position)

data GameAction = MoveLeft | MoveRight | Jump | StopJump | CancelLeft | CancelRight deriving (Show, Eq)

data PhysicsAction = Impulse { impulseVx::Double, impulseVy::Double }

data AnimationAction = StartClip { _clip :: AnimationClip }

data GameEvent event = GameEvent { _identifier :: GameEntityIdentifier, _gameEvent :: event }

data GameState = GameState { worldState :: WorldState,
                             _resources :: GraphicResources,
                             actorStates :: ActorStates,
                             physicsState::PhysicsState,
                             boundingBoxState :: BoundingBoxState,
                             _animationStates :: AnimationStates,
                             renderingHandlers :: RenderingHandlers,
                             _players :: HaskQuery.Relation Player (),
                             _font :: Maybe Font,
                             _currentGameTime :: Int }


emptyGameState = GameState { worldState = Data.IntMap.Lazy.empty,
                              _resources = Data.IntMap.Lazy.empty,
                              actorStates = Data.IntMap.Lazy.empty,
                              physicsState = Data.IntMap.Lazy.empty,
                              _animationStates = Data.IntMap.Lazy.empty,
                              boundingBoxState = Data.IntMap.Lazy.empty,
                              renderingHandlers = Data.IntMap.Lazy.empty,
                              _players = HaskQuery.empty,
                              _font = Nothing ,
                              _currentGameTime = 0}


data GameEventQueues = GameEventQueues {
    gameActions :: [GameEvent GameAction],
    physicsActions :: [GameEvent PhysicsAction],
    animationActions :: [GameEvent AnimationAction]
}

emptyGameEventQueues = GameEventQueues {
    gameActions = [],
    physicsActions = [],
    animationActions = []
}

data MenuState = MenuState {
        menuPosition :: Integer,
        menuItems :: [String],
        menuFont :: Font
    } deriving (Eq)

data MenuAction = MoveSelectionUp | MoveSelectionDown | SelectItem deriving (Eq, Show)

data ActorState = Idle | MovingLeft | MovingRight | Jumping | JumpFall

type ActorStates = Data.IntMap.Lazy.IntMap ActorState

type DrawAction = Int -> Int -> Graphics.UI.SDL.Types.Surface -> IO ()


