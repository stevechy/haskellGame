module HaskellGame.Rendering.Renderer where

import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Primitives
import Graphics.UI.SDL.Video
import Graphics.UI.SDL.Types
import Graphics.UI.SDL.TTF
import Graphics.UI.SDL.Time
import Data.IntMap.Lazy
import qualified Data.Array
import HaskellGame.Types

drawWorld  :: Graphics.UI.SDL.Types.Surface -> GameState -> IO ()
drawWorld videoSurface gameState = foldrWithKey (\key value seed -> ((value key gameState videoSurface) >> seed)) (return ()) $ renderingHandlers gameState

bbDrawRect :: Position -> BoundingBox -> Rect
bbDrawRect pos bb = Rect posx posy (posx + (boxWidth bb)) (posy+(boxHeight bb)) 
  where posx = (_x pos) + (relX bb)
        posy = (_y pos) + (relY bb)

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
      _ <- Graphics.UI.SDL.Primitives.pixel videoSurface (fromIntegral (_x position)) (fromIntegral (_y position)) rad
      return ()
    _ -> return ()
  
  
characterRender :: RenderingHandler
characterRender key gameState videoSurface = do
  let worldStateElement = worldState gameState
  let graphicsElement = _resources gameState
  let red = SDL.Pixel 0xFF0000FF
  let rad = SDL.Pixel 0xFFF000FF
  let Just (Position x y) = Data.IntMap.Lazy.lookup key worldStateElement
  let Just (Image image) = Data.IntMap.Lazy.lookup key graphicsElement 
  let Just (BoundingBox 0 0 w h) = Data.IntMap.Lazy.lookup key $ boundingBoxState gameState  
  _ <- blitSurface image Nothing videoSurface (Just (Rect x y w h))
  _ <- Graphics.UI.SDL.Primitives.rectangle videoSurface (Rect x y (x+w) (y+h)) red 
  _ <- Graphics.UI.SDL.Primitives.pixel videoSurface (fromIntegral x) (fromIntegral y) rad
  return ();

animatedRender :: RenderingHandler
animatedRender key gameState videoSurface = do
  let worldStateElement = worldState gameState
  let graphicsElement = _resources gameState
  let red = SDL.Pixel 0xFF0000FF
  let rad = SDL.Pixel 0xFFF000FF
  let Just (Position x y) = Data.IntMap.Lazy.lookup key worldStateElement
  let Just (AnimationClip {_startTime = startTime, _rate = rate}) = Data.IntMap.Lazy.lookup key $ _animationStates gameState
  currentTicks <- Graphics.UI.SDL.Time.getTicks 
  let currentTime = (fromIntegral $ currentTicks) - startTime
  let Just (ImageSet imageSet) = Data.IntMap.Lazy.lookup key graphicsElement
  let Just (BoundingBox 0 0 w h) = Data.IntMap.Lazy.lookup key $ boundingBoxState gameState 
  let frameNumber = (currentTime `quot` rate) `rem`  ((snd $ Data.Array.bounds imageSet) + 1)
  _ <- blitSurface (imageSet Data.Array.! frameNumber)  Nothing videoSurface (Just (Rect x y w h))  
  _ <- Graphics.UI.SDL.Primitives.rectangle videoSurface (Rect x y (x+w) (y+h)) red 
  _ <- Graphics.UI.SDL.Primitives.pixel videoSurface (fromIntegral x) (fromIntegral y) rad
  return ();

drawGame :: Graphics.UI.SDL.Types.Surface -> GameState -> IO ()
drawGame videoSurface gameState = do  
  let black = SDL.Pixel 0x00000000  
  _ <- Graphics.UI.SDL.Video.fillRect videoSurface Nothing black
  message <- renderTextBlended (_font gameState) "Score" $ Color 255 0 0
  rect <- getClipRect message
  _ <- blitSurface message Nothing videoSurface $ Just $ rect { rectX = 50, rectY=0}
  HaskellGame.Rendering.Renderer.drawWorld videoSurface gameState
  _ <- tryFlip videoSurface
  return ()

