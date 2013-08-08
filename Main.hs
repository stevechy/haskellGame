module Main where

import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Events 
import Graphics.UI.SDL.Types
import Graphics.UI.SDL.Time
import Graphics.UI.SDL.Video
import Graphics.UI.SDL.Image
import Graphics.UI.SDL.Keysym as Keysym
import Data.List
import Data.Maybe

import Data.IntMap.Lazy

main :: IO ()
main = withInit [InitEverything] runGame

data Position = Position { x :: Int, y :: Int } 

playerId :: Int
playerId = 1

type WorldState = Data.IntMap.Lazy.IntMap Position

initialState :: WorldState
initialState = Data.IntMap.Lazy.fromList [(playerId, Position 5 5)]

type GraphicResources = Data.IntMap.Lazy.IntMap Graphics.UI.SDL.Types.Surface

data GameAction = MoveLeft | MoveRight deriving (Show, Eq)

data GameState = GameState { worldState :: WorldState, resources :: GraphicResources }

loadResources :: IO (GraphicResources)
loadResources = do  
  image <- Graphics.UI.SDL.Image.load "PlanetCute PNG/Character Boy.png"
  return $ Data.IntMap.Lazy.fromList [(playerId, image)]

runGame :: IO ()
runGame = do
  Just videoSurface <- Graphics.UI.SDL.Video.trySetVideoMode 640 480 32 []
  resources <- loadResources
  
  let gameState = GameState { worldState = initialState, resources = resources}
  
  let eventAction = Graphics.UI.SDL.Events.pollEvent
  let drawAction = drawGame videoSurface 
  
  gameLoop drawAction eventAction gameState
  
  return ()
  
pollEvents eventAction events = do
  event <- eventAction
  case event of
    NoEvent -> return events
    ev -> pollEvents eventAction (ev:events)
  
gameLoop drawAction eventAction gameState = do
 
  events <- pollEvents eventAction []
  let gameEvents = concat $ Data.List.map gameAction events
  
  let state = isNothing $ find (\x -> x == Graphics.UI.SDL.Events.Quit) events 
  
  let processedGameState = processGameState gameState gameEvents
  drawAction processedGameState

  case state of
    True -> do 
      Graphics.UI.SDL.Time.delay 250
      gameLoop drawAction eventAction processedGameState
    False -> return ()
    
processGameState gameState events = 
  Data.List.foldr processEvent gameState events 
    where processEvent event gameState = 
            case event of
              MoveRight -> gameState { worldState = Data.IntMap.Lazy.adjust (\ (Position x y) -> Position (x+5) y) playerId (worldState gameState) }
              MoveLeft -> gameState { worldState = Data.IntMap.Lazy.adjust (\ (Position x y) -> Position (x-5) y) playerId (worldState gameState) }
                                    
    
gameAction :: Graphics.UI.SDL.Events.Event -> [GameAction]   
gameAction event = case event of
  KeyDown keysym -> 
    case Keysym.symKey keysym of
      Keysym.SDLK_RIGHT -> [MoveRight]
      Keysym.SDLK_LEFT -> [MoveLeft]
      _ -> []
  _ -> []
     

drawGame :: Graphics.UI.SDL.Types.Surface -> GameState -> IO ()
drawGame videoSurface gameState = do
  let pixelFormat = Graphics.UI.SDL.Types.surfaceGetPixelFormat videoSurface
  black <- Graphics.UI.SDL.Video.mapRGB pixelFormat 0 0 0 
  Graphics.UI.SDL.Video.fillRect videoSurface Nothing black
  let worldStateElement = worldState gameState
  let graphicsElement = resources gameState
  let Just (Position x y) = Data.IntMap.Lazy.lookup playerId worldStateElement
  let Just (image) = Data.IntMap.Lazy.lookup playerId graphicsElement
  blitSurface image Nothing videoSurface (Just (Rect x y 101 171))
  tryFlip videoSurface
  return ()