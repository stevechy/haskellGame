module HaskellGame.Rendering.Renderer where

import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Primitives
import Graphics.UI.SDL.Video
import Graphics.UI.SDL.Types
import Data.IntMap.Lazy
import HaskellGame.Types

drawWorld  :: Graphics.UI.SDL.Types.Surface -> GameState -> IO ()
drawWorld videoSurface gameState = foldrWithKey (\key value seed -> ((value key gameState videoSurface) >> seed)) (return ()) $ renderingHandlers gameState

bbDrawRect :: Position -> BoundingBox -> Rect
bbDrawRect pos bb = Rect posx posy (posx + (boxWidth bb)) (posy+(boxHeight bb)) 
  where posx = (x pos) + (relX bb)
        posy = (y pos) + (relY bb)

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