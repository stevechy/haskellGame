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
import Control.Monad
import Data.IntMap.Lazy

import HaskellGame.Types
import HaskellGame.Game

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
  player <- Graphics.UI.SDL.Image.load "Platformer Art Complete Pack_0/Base pack/Player/p1_front.png"
  return $ Data.IntMap.Lazy.fromList [(7, image), (playerId, player)]

runGame :: IO ()
runGame = do
  _ <- Graphics.UI.SDL.TTF.init
  font <- openFont "Fonts/SourceSansPro-Black.ttf" 20
  Just videoSurface <- Graphics.UI.SDL.Video.trySetVideoMode 800 540 32 [ DoubleBuf]
  resources <- loadResources

  let menuState = MenuState { menuPosition = 0, menuItems = ["Start Game","Options","Quit"], menuFont = font }
  runMenu menuState videoSurface
  
  let gameState = initializeGameState $ GameState { worldState = initialState, 
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

initializeGameState gameState = 
    insertComponents 99 [CollisionComponent (BoundingBox 0 0 10 10), PositionComponent (Position 300 5),  RenderingComponent HaskellGame.Rendering.Renderer.rectRenderer ] gameState

runMenu :: MenuState -> Surface -> IO ()
runMenu menuState videoSurface = do
    events <- pollEvents Graphics.UI.SDL.Events.pollEvent []
    let menuActions = concat $ Data.List.map menuAction events
    let state = (isNothing $ find (\x -> x == Graphics.UI.SDL.Events.Quit) events ) && (isNothing $ find (\x -> x == SelectItem) menuActions) 

    let newMenuState = Data.List.foldr processMenuAction menuState menuActions 
    case state of
        True -> do 
            let black = SDL.Pixel 0x00000000  
            _ <- Graphics.UI.SDL.Video.fillRect videoSurface Nothing black
            _ <- forM_ (Data.List.zip (menuItems newMenuState) [0,1..]) (\ (menuItem, index) -> do
                let color = if index == (menuPosition newMenuState) then Color 0 0 255 else Color 255 0 0
                message <- renderTextBlended (menuFont newMenuState) menuItem color
                rect <- getClipRect message
                _ <- blitSurface message Nothing videoSurface $ Just $ rect { rectX = 50, rectY= 50 + (20 * (fromIntegral index))}
                return ()
                )
            _ <- tryFlip videoSurface
            Graphics.UI.SDL.Time.delay 30
            runMenu newMenuState videoSurface
        False -> return ()

processMenuAction :: MenuAction -> MenuState -> MenuState
processMenuAction menuAction menuState =
    let menuSize = fromIntegral $ Data.List.length $ menuItems menuState in 
        case menuAction of
            MoveSelectionUp -> menuState { menuPosition = (((menuPosition menuState) - 1) + menuSize ) `mod` menuSize } 
            MoveSelectionDown -> menuState { menuPosition = ((menuPosition menuState) + 1) `mod` menuSize} 
            _ -> menuState
    
  
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

menuAction :: Graphics.UI.SDL.Events.Event -> [MenuAction]   
menuAction event = case event of
  KeyDown keysym -> 
    case Keysym.symKey keysym of
      Keysym.SDLK_UP -> [MoveSelectionUp]
      Keysym.SDLK_DOWN -> [MoveSelectionDown]
      Keysym.SDLK_RETURN -> [SelectItem]
      _ -> []
  KeyUp keysym ->
    case Keysym.symKey keysym of     
      _ -> []
  _ -> []          


drawGame :: Graphics.UI.SDL.Types.Surface -> GameState -> IO ()
drawGame videoSurface gameState = do  
  let black = SDL.Pixel 0x00000000  
  _ <- Graphics.UI.SDL.Video.fillRect videoSurface Nothing black
  message <- renderTextBlended (font gameState) "Score" $ Color 255 0 0
  rect <- getClipRect message
  _ <- blitSurface message Nothing videoSurface $ Just $ rect { rectX = 50, rectY=0}
  HaskellGame.Rendering.Renderer.drawWorld videoSurface gameState
  _ <- tryFlip videoSurface
  return ()


