module HaskellGame.HumanInterface.Manager
where

import Graphics.UI.SDL.Events 
import HaskellGame.Types
import Graphics.UI.SDL.Keysym as Keysym
import qualified Data.List


pollEvents :: IO (Event) -> [Event] -> IO [Event]  
pollEvents eventAction events = do
  event <- eventAction
  case event of
    NoEvent -> return events
    ev -> pollEvents eventAction (ev:events)

sdlPollEvents :: IO [Event]
sdlPollEvents = pollEvents Graphics.UI.SDL.Events.pollEvent []


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

     

