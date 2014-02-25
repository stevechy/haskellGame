module HaskellGame.Resources.ResourceManager
where

import Graphics.UI.SDL.Image
import HaskellGame.Types
import qualified Data.IntMap.Lazy

loadResources :: GameEntityIdentifier -> IO (GraphicResources)
loadResources playerId = do  
  image <- Graphics.UI.SDL.Image.load "lambdaChar.png"
  player <- Graphics.UI.SDL.Image.load "Platformer Art Complete Pack_0/Base pack/Player/p1_front.png"
  
  return $ Data.IntMap.Lazy.fromList [(7, Image image), (playerId, Image player)]

