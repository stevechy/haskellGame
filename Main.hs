module Main where

import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Events 
import Graphics.UI.SDL.Types
import Graphics.UI.SDL.Time
import Graphics.UI.SDL.Video
import Graphics.UI.SDL.Image
import Graphics.UI.SDL.Keysym as Keysym
import Graphics.UI.SDL.Primitives
import Graphics.UI.SDL.TTF
import Data.List
import Data.Maybe

import Data.IntMap.Lazy

import HaskellGame.Types

import qualified HaskellGame.Physics.Simulator
import qualified HaskellGame.Physics.CollisionDetector
import qualified HaskellGame.Rendering.Renderer
import qualified HaskellGame.Gameplay.Simulator

main :: IO ()
main = withInit [InitEverything] runGame

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
initialRenderingHandlers = Data.IntMap.Lazy.fromList [(floorId, HaskellGame.Rendering.Renderer.rectRenderer),
                                                          (platformId, HaskellGame.Rendering.Renderer.rectRenderer),
                                                          (playerId, HaskellGame.Rendering.Renderer.characterRender)] 

initialBoundingBoxState :: BoundingBoxState
initialBoundingBoxState = Data.IntMap.Lazy.fromList [(playerId, BoundingBox 0 0 101 155), (floorId, BoundingBox 0 0 640 10), (platformId, BoundingBox (-25) (-25) 50 50)]


loadResources :: IO (GraphicResources)
loadResources = do  
  image <- Graphics.UI.SDL.Image.load "lambdaChar.png"
  return $ Data.IntMap.Lazy.fromList [(playerId, image)]

runGame :: IO ()
runGame = do
  _ <- Graphics.UI.SDL.TTF.init
  font <- openFont "Fonts/SourceSansPro-Black.ttf" 20
  Just videoSurface <- Graphics.UI.SDL.Video.trySetVideoMode 800 540 32 [ DoubleBuf]
  resources <- loadResources
  
  let gameState = GameState { worldState = initialState, 
                              resources = resources, 
                              actorStates = initialActorStates, 
                              physicsState = initialPhysicsState, 
                              boundingBoxState = initialBoundingBoxState,
                              renderingHandlers = initialRenderingHandlers,
                              font = font}
  
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
                   [ (HaskellGame.Gameplay.Simulator.processGameState gameEvents playerId), 
                     HaskellGame.Physics.Simulator.simulateGameStatePhysics, 
                     HaskellGame.Physics.Simulator.applyPhysics delay, 
                     HaskellGame.Physics.CollisionDetector.detectAndResolveCollisions delay
                   ]        
  
  
  _ <- drawAction finalState

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

       


drawGame :: Graphics.UI.SDL.Types.Surface -> GameState -> IO ()
drawGame videoSurface gameState = do  
  let black = SDL.Pixel 0x00000000  
  _ <- Graphics.UI.SDL.Video.fillRect videoSurface Nothing black
  message <- renderTextSolid (font gameState) "Hi" $ Color 255 0 0
  rect <- getClipRect message
  _ <- blitSurface message Nothing videoSurface $ Just $ rect { rectX = 50, rectY=50}
  HaskellGame.Rendering.Renderer.drawWorld videoSurface gameState
  _ <- tryFlip videoSurface
  return ()


