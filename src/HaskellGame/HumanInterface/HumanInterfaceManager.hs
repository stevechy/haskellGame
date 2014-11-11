module HaskellGame.HumanInterface.HumanInterfaceManager
where

import Graphics.UI.SDL.Events
import HaskellGame.Types
import Graphics.UI.SDL.Keysym as Keysym
import qualified HaskQuery


pollEvents :: IO (Event) -> [Event] -> IO [Event]
pollEvents eventAction events = do
  event <- eventAction
  case event of
    NoEvent -> return events
    ev -> pollEvents eventAction (ev:events)

sdlPollEvents :: IO [Event]
sdlPollEvents = pollEvents Graphics.UI.SDL.Events.pollEvent []



playerGameAction :: GameState -> Graphics.UI.SDL.Events.Event -> [GameEvent GameAction]
playerGameAction  =  gameAction

gameAction :: GameState -> Graphics.UI.SDL.Events.Event -> [GameEvent GameAction]
gameAction gameState event = case event of
  KeyDown keysym ->
    case Keysym.symKey keysym of
      Keysym.SDLK_RIGHT -> actionForPlayer gameState 1 MoveRight
      Keysym.SDLK_LEFT -> actionForPlayer gameState 1 MoveLeft
      Keysym.SDLK_UP -> actionForPlayer gameState 1 Jump
      Keysym.SDLK_d -> actionForPlayer gameState 2 MoveRight
      Keysym.SDLK_a -> actionForPlayer gameState 2 MoveLeft
      Keysym.SDLK_w -> actionForPlayer gameState 2 Jump
      _ -> []
  KeyUp keysym ->
    case Keysym.symKey keysym of
      Keysym.SDLK_RIGHT -> actionForPlayer gameState 1 CancelRight
      Keysym.SDLK_LEFT -> actionForPlayer gameState 1 CancelLeft
      Keysym.SDLK_UP -> actionForPlayer gameState 1 StopJump
      Keysym.SDLK_d -> actionForPlayer gameState 2 CancelRight
      Keysym.SDLK_a -> actionForPlayer gameState 2 CancelLeft
      Keysym.SDLK_w -> actionForPlayer gameState 2 StopJump
      _ -> []
  _ -> []

actionForPlayer :: GameState -> Int -> GameAction -> [GameEvent GameAction]
actionForPlayer gameState playerId gameEvent =  HaskQuery.runQuery $ do
                              player <- HaskQuery.select (_players gameState)
                              _ <- HaskQuery.filter ( (_playerId player) == playerId)
                              return GameEvent{_identifier = (_playerObjectIdentifier player), _gameEvent= gameEvent}

