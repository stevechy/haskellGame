module HaskellGame.Menu.Manager
where

import qualified Graphics.UI.SDL 
import qualified Graphics.UI.SDL.Events 
import qualified Graphics.UI.SDL.Video 
import qualified Graphics.UI.SDL.TTF
import qualified Graphics.UI.SDL.Time
import Graphics.UI.SDL.Keysym as Keysym
import qualified HaskellGame.HumanInterface.Manager
import HaskellGame.Types
import qualified Data.List
import qualified Data.Maybe

import Control.Monad

runMenu :: HaskellGame.Types.MenuState -> Graphics.UI.SDL.Surface -> IO ()
runMenu menuState videoSurface = do
    events <- HaskellGame.HumanInterface.Manager.sdlPollEvents
    let menuActions = concat $ Data.List.map toMenuActions events
    let state = (Data.Maybe.isNothing $ Data.List.find (\x -> x == Graphics.UI.SDL.Events.Quit) events ) && (Data.Maybe.isNothing $ Data.List.find (\x -> x == SelectItem) menuActions) 

    let newMenuState = Data.List.foldr processMenuAction menuState menuActions 
    case state of
        True -> do 
            let black = Graphics.UI.SDL.Pixel 0x00000000  
            _ <- Graphics.UI.SDL.Video.fillRect videoSurface Nothing black
            _ <- forM_ (Data.List.zip (menuItems newMenuState) [0,1..]) (\ (menuItem, index) -> do
                let color = if index == (menuPosition newMenuState) then Graphics.UI.SDL.Color 0 0 255 else Graphics.UI.SDL.Color 255 0 0
                message <- Graphics.UI.SDL.TTF.renderTextBlended (HaskellGame.Types.menuFont newMenuState) menuItem color
                rect <- Graphics.UI.SDL.Video.getClipRect message
                _ <- Graphics.UI.SDL.Video.blitSurface message Nothing videoSurface $ Just $ rect { Graphics.UI.SDL.rectX = 50, Graphics.UI.SDL.rectY= 50 + (20 * (fromIntegral index))}
                return ()
                )
            _ <- Graphics.UI.SDL.Video.tryFlip videoSurface
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
    

toMenuActions :: Graphics.UI.SDL.Events.Event -> [MenuAction]   
toMenuActions event = case event of
  Graphics.UI.SDL.Events.KeyDown keysym -> 
    case Keysym.symKey keysym of
      Keysym.SDLK_UP -> [MoveSelectionUp]
      Keysym.SDLK_DOWN -> [MoveSelectionDown]
      Keysym.SDLK_RETURN -> [SelectItem]
      _ -> []
  Graphics.UI.SDL.Events.KeyUp keysym ->
    case Keysym.symKey keysym of     
      _ -> []
  _ -> []     