module HaskellGame.Types where

import Data.IntMap.Lazy

import Graphics.UI.SDL.Types


data Position = Position { x :: Int, y :: Int} deriving Show


data VelocityAcceleration = VelocityAcceleration { vx::Double, vy::Double, ax::Double, ay::Double}

data BoundingBox = BoundingBox { relX :: Int, relY :: Int, boxWidth :: Int, boxHeight::Int} deriving Show

type BoundingBoxState = Data.IntMap.Lazy.IntMap BoundingBox

type PhysicsState = Data.IntMap.Lazy.IntMap VelocityAcceleration

type WorldState = Data.IntMap.Lazy.IntMap Position

type RenderingHandler = Int -> GameState -> Surface -> IO ()

type RenderingHandlers = Data.IntMap.Lazy.IntMap RenderingHandler

type GraphicResources = Data.IntMap.Lazy.IntMap Graphics.UI.SDL.Types.Surface

type CollisionUnit = (Int, BoundingBox, Position)

data GameAction = MoveLeft | MoveRight | Jump | StopJump | CancelLeft | CancelRight deriving (Show, Eq)

data GameState = GameState { worldState :: WorldState, 
                             resources :: GraphicResources, 
                             actorStates :: ActorStates, 
                             physicsState::PhysicsState, 
                             boundingBoxState :: BoundingBoxState,
                             renderingHandlers :: RenderingHandlers }

data ActorState = Idle | MovingLeft | MovingRight | Jumping

type ActorStates = Data.IntMap.Lazy.IntMap ActorState

type DrawAction = Int -> Int -> Graphics.UI.SDL.Types.Surface -> IO ()