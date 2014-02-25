module Main where

import qualified Graphics.UI.SDL 
import Graphics.UI.SDL.Events 
import Graphics.UI.SDL.Time
import Graphics.UI.SDL.Video
import Graphics.UI.SDL.Keysym as Keysym
import Graphics.UI.SDL.TTF
import GHC.Word
import Data.List
import qualified Data.Maybe
import Data.IntMap.Lazy

import HaskellGame.Types
import HaskellGame.Game

import qualified HaskellGame.Physics.Simulator
import qualified HaskellGame.Physics.CollisionDetector
import qualified HaskellGame.Rendering.Renderer
import qualified HaskellGame.Gameplay.Simulator
import qualified HaskellGame.Resources.ResourceManager
import qualified HaskellGame.HumanInterface.Manager
import qualified HaskellGame.Menu.Manager

main :: IO ()
main = Graphics.UI.SDL.withInit [Graphics.UI.SDL.InitEverything] runGame

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



runGame :: IO ()
runGame = do
  _ <- Graphics.UI.SDL.TTF.init
  font <- openFont "Fonts/SourceSansPro-Black.ttf" 20
  Just videoSurface <- Graphics.UI.SDL.Video.trySetVideoMode 800 540 32 [ Graphics.UI.SDL.DoubleBuf]
  resources <- HaskellGame.Resources.ResourceManager.loadResources playerId

  let menuState = MenuState { menuPosition = 0, menuItems = ["Start Game","Options","Quit"], menuFont = font }
  HaskellGame.Menu.Manager.runMenu menuState videoSurface
  
  let gameState = initializeGameState $ GameState { worldState = initialState, 
                              _resources = resources, 
                              actorStates = initialActorStates, 
                              physicsState = initialPhysicsState, 
                              boundingBoxState = initialBoundingBoxState,
                              renderingHandlers = initialRenderingHandlers,
                              _font = font}
  
  
  let eventAction = Graphics.UI.SDL.Events.pollEvent
  let drawAction = HaskellGame.Rendering.Renderer.drawGame videoSurface 
  initialTicks <- Graphics.UI.SDL.Time.getTicks
    
  gameLoop drawAction eventAction gameState initialTicks
  
  return ()

initializeGameState :: GameState -> GameState
initializeGameState gameState = 
    insertEntity 99 [CollisionComponent (BoundingBox 0 0 10 10), PositionComponent (Position 300 5),  RenderingComponent HaskellGame.Rendering.Renderer.rectRenderer ] gameState



  
gameLoop :: (GameState -> IO t) -> IO Event -> GameState -> GHC.Word.Word32 -> IO ()
gameLoop drawAction eventAction gameState lastFrameTicks = do
 
  events <- HaskellGame.HumanInterface.Manager.pollEvents eventAction []
  let gameEvents = GameEventQueues { gameActions = concat $ Data.List.map (playerGameAction playerId) events,
                                  physicsActions = [] }
  
  let state = Data.Maybe.isNothing $ find (\x -> x == Graphics.UI.SDL.Events.Quit) events 
  
  currentTicks <- Graphics.UI.SDL.Time.getTicks
  
  let frameDelay = fromIntegral $ currentTicks - lastFrameTicks
   
  let (finalState, finalQueues) = 
        Data.List.foldl' ( \ currentGameState gameStep -> gameStep currentGameState) (gameState, gameEvents) 
                   [ HaskellGame.Gameplay.Simulator.processGameStateOutputEvents, 
                     HaskellGame.Physics.Simulator.applyPhysicsChanges, 
                     HaskellGame.Physics.Simulator.applyPhysics frameDelay, 
                     HaskellGame.Physics.CollisionDetector.detectAndResolveCollisions frameDelay
                   ]        
  
  
  _ <- drawAction finalState

  case state of
    True -> do 
      Graphics.UI.SDL.Time.delay 30
      gameLoop drawAction eventAction finalState currentTicks
    False -> return ()

playerGameAction :: GameEntityIdentifier -> Graphics.UI.SDL.Events.Event -> [GameEvent GameAction]   
playerGameAction entityId =  (Data.List.map (\ev -> GameEvent { _identifier = entityId, _gameEvent =  ev})) . gameAction 
    
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

     




