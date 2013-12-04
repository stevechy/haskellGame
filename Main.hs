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

import Types

main :: IO ()
main = withInit [InitEverything] runGame



bbDrawRect :: Position -> BoundingBox -> Rect
bbDrawRect pos bb = Rect posx posy (posx + (boxWidth bb)) (posy+(boxHeight bb)) 
  where posx = (Types.x pos) + (relX bb)
        posy = (Types.y pos) + (relY bb)

rectRenderer :: RenderingHandler
rectRenderer key gameState videoSurface = do
  case Data.IntMap.Lazy.lookup key $ worldState gameState of
    Just position -> do
      let red = SDL.Pixel 0xFF0000FF
      let rad = SDL.Pixel 0xFFF000FF
      let boundingBox = Data.IntMap.Lazy.lookup key $ boundingBoxState gameState    
      _ <- case boundingBox of 
        Just bb -> Graphics.UI.SDL.Primitives.box videoSurface (bbDrawRect position bb) red
        Nothing -> return False
      _ <- Graphics.UI.SDL.Primitives.pixel videoSurface (fromIntegral (x position)) (fromIntegral (y position)) rad
      return ()
    _ -> return ()
  
  
characterRender :: RenderingHandler
characterRender key gameState videoSurface = do
  let worldStateElement = worldState gameState
  let graphicsElement = resources gameState
  let red = SDL.Pixel 0xFF0000FF
  let rad = SDL.Pixel 0xFFF000FF
  let Just (Position x y) = Data.IntMap.Lazy.lookup key worldStateElement
  let Just (image) = Data.IntMap.Lazy.lookup key graphicsElement
  _ <- blitSurface image Nothing videoSurface (Just (Rect x y 101 171))
  _ <- Graphics.UI.SDL.Primitives.rectangle videoSurface (Rect x y (x+101) (y+171)) red 
  _ <- Graphics.UI.SDL.Primitives.pixel videoSurface (fromIntegral x) (fromIntegral y) rad
  return ();


playerId :: Int
playerId = 1

floorId :: Int
floorId = 2

platformId :: Int
platformId = 3

initialState :: WorldState
initialState = Data.IntMap.Lazy.fromList [(playerId, Position 5 5), (floorId, Position 0 400), (platformId, Position 500 300)]

initialActorStates :: ActorStates
initialActorStates = Data.IntMap.Lazy.fromList [(playerId, Idle)]

initialPhysicsState :: PhysicsState
initialPhysicsState = Data.IntMap.Lazy.fromList [(playerId, VelocityAcceleration {vx = 0, vy = 0.00, ax = 0, ay = 0.0002})]

initialRenderingHandlers :: RenderingHandlers
initialRenderingHandlers = Data.IntMap.Lazy.fromList [(floorId, rectRenderer),(platformId, rectRenderer),(playerId, characterRender)] 

initialBoundingBoxState :: BoundingBoxState
initialBoundingBoxState = Data.IntMap.Lazy.fromList [(playerId, BoundingBox 0 0 101 155), (floorId, BoundingBox 0 0 640 10), (platformId, BoundingBox (-25) (-25) 50 50)]


loadResources :: IO (GraphicResources)
loadResources = do  
  image <- Graphics.UI.SDL.Image.load "lambdaChar.png"
  return $ Data.IntMap.Lazy.fromList [(playerId, image)]

runGame :: IO ()
runGame = do
  Just videoSurface <- Graphics.UI.SDL.Video.trySetVideoMode 800 540 32 [ DoubleBuf]
  resources <- loadResources
  
  let gameState = GameState { worldState = initialState, 
                              resources = resources, 
                              actorStates = initialActorStates, 
                              physicsState = initialPhysicsState, 
                              boundingBoxState = initialBoundingBoxState,
                              renderingHandlers = initialRenderingHandlers}
  
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
  
  let delay = fromIntegral $ currentTicks - lastFrameTicks
   
  let finalState = 
        Data.List.foldl' ( \ gameState gameStep -> gameStep gameState) gameState 
                   [ (processGameState gameEvents), 
                     simulateGameStatePhysics, 
                     applyPhysics delay, 
                     detectAndResolveCollisions delay
                   ]        
  
  
  drawAction finalState

  case state of
    True -> do 
      Graphics.UI.SDL.Time.delay 30
      gameLoop drawAction eventAction finalState currentTicks
    False -> return ()
    
gameAction :: Graphics.UI.SDL.Events.Event -> [GameAction]   
gameAction event = case event of
  KeyDown keysym -> 
    case Keysym.symKey keysym of
      Keysym.SDLK_RIGHT -> [MoveRight]
      Keysym.SDLK_LEFT -> [MoveLeft]
      Keysym.SDLK_UP -> [Jump]
      _ -> []
  KeyUp keysym ->
    case Keysym.symKey keysym of
      Keysym.SDLK_RIGHT -> [CancelRight]
      Keysym.SDLK_LEFT -> [CancelLeft]
      Keysym.SDLK_UP -> [StopJump]
      _ -> []
  _ -> []
    
processGameState events gameState = 
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
       
collisionProduct :: [a] -> [(a,a)]
collisionProduct (x:xs) = Data.List.foldr (\ item seed -> (x,item) : seed)  (collisionProduct xs) xs
collisionProduct [] = []
       


boundingBoxList :: GameState -> [CollisionUnit]
boundingBoxList gameState = foldrWithKey bbWithPosition [] $ boundingBoxState gameState
  where worldStateItem = worldState gameState
        bbWithPosition key value seed = case Data.IntMap.Lazy.lookup key worldStateItem of
          Just pos -> (key, value, pos) : seed
          Nothing -> seed
          
collides :: (CollisionUnit, CollisionUnit) -> Bool
collides ((idA,bbA,posA),(idB,bbB,posB)) = ((xa1 <= xb1 && xb1 <= xa2) || (xb1 <= xa1 && xa1 <= xb2)) && ((ya1 <= yb1 && yb1 <= ya2) || (yb1 <= ya1 && ya1 <= yb2))
  where xa1 = (x posA) +  (relX bbA)
        xa2 = xa1 + (boxWidth bbA)
        xb1 = (x posB) + (relX bbB)
        xb2 = xb1 + (boxWidth bbB)
        ya1 = (y posA) + (relY bbA)
        ya2 = ya1 + (boxHeight bbA)
        yb1 = (y posB) + (relY bbB)
        yb2 = yb1 + (boxHeight bbB)

detectAndResolveCollisions :: Int -> GameState -> GameState
detectAndResolveCollisions delta gameState = Data.List.foldl' respondToCollision gameState (collisions gameState)
                                             
respondToCollision gameStateSeed  ((idA,bbA,posA),(idB,bbB, posB)) 
  | idA == playerId = gameStateSeed { physicsState = Data.IntMap.Lazy.adjust (\phys -> phys { vy = 0})  idA (physicsState gameStateSeed) }
  | idB == playerId = gameStateSeed { physicsState = Data.IntMap.Lazy.adjust (\phys -> phys { vy = 0})  idB (physicsState gameStateSeed) }
  | True = gameStateSeed
  
collisions :: GameState -> [(CollisionUnit, CollisionUnit)]
collisions gameState =   
  let collisionsToTest = collisionProduct $ boundingBoxList gameState
  in  Data.List.filter collides collisionsToTest
                                             

drawGame :: Graphics.UI.SDL.Types.Surface -> GameState -> IO ()
drawGame videoSurface gameState = do  
  let black = SDL.Pixel 0x00000000  
  _ <- Graphics.UI.SDL.Video.fillRect videoSurface Nothing black

  drawWorld videoSurface gameState
  _ <- tryFlip videoSurface
  return ()

drawWorld  :: Graphics.UI.SDL.Types.Surface -> GameState -> IO ()
drawWorld videoSurface gameState = foldrWithKey (\key value seed -> ((value key gameState videoSurface) >> seed)) (return ()) $ renderingHandlers gameState

