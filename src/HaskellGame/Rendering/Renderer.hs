module HaskellGame.Rendering.Renderer where

import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Primitives
import Graphics.UI.SDL.Video
import Graphics.UI.SDL.Types
import Graphics.UI.SDL.TTF
import Graphics.UI.SDL.Time
import qualified Data.IntMap.Lazy
import qualified Data.Array
import HaskellGame.Types
import qualified HaskQuery

drawWorld  :: Graphics.UI.SDL.Types.Surface -> Camera -> GameState -> IO ()
drawWorld videoSurface camera gameState = Data.IntMap.Lazy.foldrWithKey (\key renderer seed -> ((renderer key (Just camera) gameState videoSurface) >> seed)) (return ()) $ renderingHandlers gameState

drawWorldWithCamera :: Graphics.UI.SDL.Types.Surface -> GameState -> IO ()
drawWorldWithCamera surface gameState = do
    HaskQuery.runQueryM $ do
        camera <- HaskQuery.selectM $ (_cameras gameState)
        _ <- HaskQuery.executeM $ do 
            drawWorld surface camera gameState
        return () 
    return ()

bbDrawRect :: Position -> BoundingBox -> Rect
bbDrawRect pos bb = Rect posx posy (posx + (boxWidth bb)) (posy+(boxHeight bb)) 
  where posx = (_x pos) + (relX bb)
        posy = (_y pos) + (relY bb)

positionRelativeToCamera :: Maybe Camera -> GameState -> Position -> Position
positionRelativeToCamera cameraMaybe gameState pos =
    case cameraMaybe of
        Just camera -> subtractPosition pos $ Data.IntMap.Lazy.findWithDefault (Position { _x =0, _y = 0}) (_cameraEntityId camera) (worldState gameState)
        Nothing     -> pos

subtractPosition pos1 pos2 = Position { _x = (_x pos1) - (_x pos2), _y = (_y pos1) - (_y pos2)}    

rectRenderer :: RenderingHandler
rectRenderer key cameraMaybe gameState videoSurface = do
  case Data.IntMap.Lazy.lookup key $ worldState gameState of
    Just position -> do
      let red = SDL.Pixel 0xFF0000FF
      let rad = SDL.Pixel 0xFFF000FF
      let  cameraRelativePosition = positionRelativeToCamera cameraMaybe gameState position
           (xPos, yPos) =  ((fromIntegral (_x cameraRelativePosition)), (fromIntegral (_y cameraRelativePosition)))
      let boundingBox = Data.IntMap.Lazy.lookup key $ boundingBoxState gameState    
      _ <- case boundingBox of 
        Just bb -> Graphics.UI.SDL.Primitives.box videoSurface (bbDrawRect cameraRelativePosition bb) red
        Nothing -> return False

     
           
      _ <- Graphics.UI.SDL.Primitives.pixel videoSurface xPos yPos rad
      return ()
    _ -> return ()
  
  
characterRender :: RenderingHandler
characterRender key cameraMaybe gameState videoSurface = do
  let worldStateElement = worldState gameState
  let graphicsElement = _resources gameState
  let red = SDL.Pixel 0xFF0000FF
  let rad = SDL.Pixel 0xFFF000FF
  let Just pos = Data.IntMap.Lazy.lookup key worldStateElement
  let (Position x y) = positionRelativeToCamera cameraMaybe gameState pos
  let Just (Image image) = Data.IntMap.Lazy.lookup key graphicsElement 
  let Just (BoundingBox 0 0 w h) = Data.IntMap.Lazy.lookup key $ boundingBoxState gameState  
  _ <- blitSurface image Nothing videoSurface (Just (Rect x y w h))
  _ <- Graphics.UI.SDL.Primitives.rectangle videoSurface (Rect x y (x+w) (y+h)) red 
  _ <- Graphics.UI.SDL.Primitives.pixel videoSurface (fromIntegral x) (fromIntegral y) rad
  return ();

animatedRender :: RenderingHandler
animatedRender key cameraMaybe gameState videoSurface = do
  let worldStateElement = worldState gameState
  let graphicsElement = _resources gameState
  let red = SDL.Pixel 0xFF0000FF
  let rad = SDL.Pixel 0xFFF000FF
  let Just pos = Data.IntMap.Lazy.lookup key worldStateElement
  let (Position x y) = positionRelativeToCamera cameraMaybe gameState pos
  let Just (AnimationClip {_startTime = startTime,_resourceId = resourceId, _rate = rate}) = Data.IntMap.Lazy.lookup key $ _animationStates gameState
   
  let currentTime = _currentGameTime gameState
  let Just (ImageSet imageSet) = Data.IntMap.Lazy.lookup resourceId graphicsElement
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
  let Just font = _font gameState
  message <- renderTextBlended font "Score" $ Color 255 0 0
  rect <- getClipRect message
  _ <- blitSurface message Nothing videoSurface $ Just $ rect { rectX = 50, rectY=0}
  HaskellGame.Rendering.Renderer.drawWorldWithCamera videoSurface gameState
  _ <- tryFlip videoSurface
  return ()

