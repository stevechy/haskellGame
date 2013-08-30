module Main where

import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Events 
import Graphics.UI.SDL.Types
import Graphics.UI.SDL.Time
import Graphics.UI.SDL.Video
import Graphics.UI.SDL.Image
import Graphics.UI.SDL.Keysym as Keysym
import Graphics.UI.SDL.Primitives
import Data.List
import Data.Maybe

import Data.IntMap.Lazy

main :: IO ()
main = withInit [InitEverything] runGame

data Position = Position { x :: Int, y :: Int} 

data VelocityAcceleration = VelocityAcceleration { vx::Double, vy::Double, ax::Double, ay::Double}


type PhysicsState = Data.IntMap.Lazy.IntMap VelocityAcceleration

type WorldState = Data.IntMap.Lazy.IntMap Position

type RenderingHandler = Int -> GameState -> Surface -> IO ()

type RenderingHandlers = Data.IntMap.Lazy.IntMap RenderingHandler

type GraphicResources = Data.IntMap.Lazy.IntMap Graphics.UI.SDL.Types.Surface

data GameAction = MoveLeft | MoveRight | CancelLeft | CancelRight deriving (Show, Eq)

data GameState = GameState { worldState :: WorldState, resources :: GraphicResources, actorStates :: ActorStates, physicsState::PhysicsState, renderingHandlers :: RenderingHandlers }

data ActorState = Idle | MovingLeft | MovingRight

type ActorStates = Data.IntMap.Lazy.IntMap ActorState

type DrawAction = Int -> Int -> Graphics.UI.SDL.Types.Surface -> IO ()


rectRenderer :: RenderingHandler
rectRenderer key gameState videoSurface = do
  case Data.IntMap.Lazy.lookup key $ worldState gameState of
    Just (Position x y) -> do
      let red = SDL.Pixel 0xFF0000FF
      _ <- Graphics.UI.SDL.Primitives.box videoSurface (Rect x y (x+640) (y+10)) red
      return ()
    _ -> return ()
  


playerId :: Int
playerId = 1

floorId :: Int
floorId = 2

initialState :: WorldState
initialState = Data.IntMap.Lazy.fromList [(playerId, Position 5 5), (floorId, Position 0 400)]

initialActorStates :: ActorStates
initialActorStates = Data.IntMap.Lazy.fromList [(playerId, Idle)]

initialPhysicsState :: PhysicsState
initialPhysicsState = Data.IntMap.Lazy.fromList [(playerId, VelocityAcceleration {vx = 0, vy = 0.1, ax = 0, ay = 0})]

initialRenderingHandlers :: RenderingHandlers
initialRenderingHandlers = Data.IntMap.Lazy.fromList [(floorId, rectRenderer)] 



loadResources :: IO (GraphicResources)
loadResources = do  
  image <- Graphics.UI.SDL.Image.load "PlanetCute PNG/Character Boy.png"
  return $ Data.IntMap.Lazy.fromList [(playerId, image)]

runGame :: IO ()
runGame = do
  Just videoSurface <- Graphics.UI.SDL.Video.trySetVideoMode 640 480 32 [ DoubleBuf]
  resources <- loadResources
  
  let gameState = GameState { worldState = initialState, resources = resources, actorStates = initialActorStates, physicsState = initialPhysicsState, renderingHandlers = initialRenderingHandlers}
  
  let eventAction = Graphics.UI.SDL.Events.pollEvent
  let drawAction = drawGame videoSurface 
  initialTicks <- Graphics.UI.SDL.Time.getTicks
    
  gameLoop drawAction eventAction gameState initialTicks
  
  return ()
  
pollEvents eventAction events = do
  event <- eventAction
  case event of
    NoEvent -> return events
    ev -> pollEvents eventAction (ev:events)
  
gameLoop drawAction eventAction gameState lastFrameTicks = do
 
  events <- pollEvents eventAction []
  let gameEvents = concat $ Data.List.map gameAction events
  
  let state = isNothing $ find (\x -> x == Graphics.UI.SDL.Events.Quit) events 
  
  currentTicks <- Graphics.UI.SDL.Time.getTicks
  
  let delay = currentTicks - lastFrameTicks
   
  let finalState = 
        Data.List.foldl' ( \ gameState gameStep -> gameStep gameState) gameState 
                   [ (processGameState gameEvents), 
                     simulateGameState, 
                     applyPhysics (fromIntegral delay) ]        
  drawAction finalState

  case state of
    True -> do 
      Graphics.UI.SDL.Time.delay 30
      gameLoop drawAction eventAction finalState currentTicks
    False -> return ()
    
processGameState events gameState = 
  Data.List.foldr processEvent gameState events 
    where processEvent event gameState = 
            gameState { actorStates = Data.IntMap.Lazy.adjust (processActorEvent event) playerId (actorStates gameState) }
            
simulateGameState gameState = gameState { worldState = Data.IntMap.Lazy.mapWithKey (simulateActor actorStatesItem) worldStateItem  }           
  where actorStatesItem = actorStates gameState
        worldStateItem = worldState gameState
  
simulateActor :: ActorStates -> Int -> Position -> Position
simulateActor actorStates actorId actorPosition =
  case (Data.IntMap.Lazy.lookup actorId actorStates, actorPosition) of
              ((Just MovingRight), (Position x y)) -> Position (x+5) y
              ((Just MovingLeft), (Position x y)) ->  Position (x-5) y
              (state, position) -> position
              
applyPhysics :: Int -> GameState -> GameState              
applyPhysics delta gameState = gameState { worldState = Data.IntMap.Lazy.mapWithKey (simulatePhysics delta physicsStateItem) worldStateItem  }
  where physicsStateItem = physicsState gameState
        worldStateItem = worldState gameState
        
simulatePhysics :: Int -> PhysicsState -> Int -> Position -> Position
simulatePhysics delta physicsData id position = 
  case (Data.IntMap.Lazy.lookup id physicsData, position) of
       ((Just (VelocityAcceleration {vx = xvel, vy = yvel, ax = xaccel, ay = yaccel})), Position x y) -> Position (x + truncate (xvel * fromIntegral delta)) (y+ truncate (yvel * fromIntegral delta))
       (state, position) -> position
                                    
processActorEvent event actorState =                          
  case (event,actorState) of
    (MoveRight, _) -> MovingRight
    (MoveLeft, _) -> MovingLeft
    (CancelRight, _) -> Idle  
    (CancelLeft, _) -> Idle
    
gameAction :: Graphics.UI.SDL.Events.Event -> [GameAction]   
gameAction event = case event of
  KeyDown keysym -> 
    case Keysym.symKey keysym of
      Keysym.SDLK_RIGHT -> [MoveRight]
      Keysym.SDLK_LEFT -> [MoveLeft]
      _ -> []
  KeyUp keysym ->
    case Keysym.symKey keysym of
      Keysym.SDLK_RIGHT -> [CancelRight]
      Keysym.SDLK_LEFT -> [CancelLeft]
      _ -> []
  _ -> []
     

drawGame :: Graphics.UI.SDL.Types.Surface -> GameState -> IO ()
drawGame videoSurface gameState = do  
  let black = SDL.Pixel 0x00000000  
  _ <- Graphics.UI.SDL.Video.fillRect videoSurface Nothing black
  let worldStateElement = worldState gameState
  let graphicsElement = resources gameState
  let Just (Position x y) = Data.IntMap.Lazy.lookup playerId worldStateElement
  let Just (image) = Data.IntMap.Lazy.lookup playerId graphicsElement
  _ <- blitSurface image Nothing videoSurface (Just (Rect x y 101 171))

  foldrWithKey (\key value seed -> ((value key gameState videoSurface) >> seed)) (return ()) $ renderingHandlers gameState
  tryFlip videoSurface
  return ()