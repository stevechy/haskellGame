module HaskellGame.Resources.ResourceManager
where

import Graphics.UI.SDL.Image
import Graphics.UI.SDL.Types
import HaskellGame.Types
import qualified Data.IntMap.Lazy
import Data.Array

imageSet :: [Graphics.UI.SDL.Types.Surface]  -> Data.Array.Array Int Graphics.UI.SDL.Types.Surface
imageSet images = listArray (0, (length images) - 1) images

loadResources :: GameEntityIdentifier -> GameEntityIdentifier -> GameEntityIdentifier -> IO (GraphicResources)
loadResources playerId walkCycleId jumpId = do  
  image <- Graphics.UI.SDL.Image.load "lambdaChar.png"
  player <- Graphics.UI.SDL.Image.load "Platformer Art Complete Pack_0/Base pack/Player/p1_front.png"
  jump <- Graphics.UI.SDL.Image.load "Platformer Art Complete Pack_0/Base pack/Player/p1_jump.png"
  let jumpImageSet = ImageSet $ imageSet [jump]
  walkCycle <- mapM (\ imageIndex -> Graphics.UI.SDL.Image.load $ "Platformer Art Complete Pack_0/Base pack/Player/p1_walk/PNG/p1_walk" ++ imageIndex ++ ".png") ["01","02","03","04","05","06","07","08","09","10"]
  return $ Data.IntMap.Lazy.fromList [(7, Image image), (playerId, Image player) , (playerId, ImageSet $ imageSet walkCycle ), (jumpId, jumpImageSet)]

