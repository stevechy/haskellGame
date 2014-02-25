module HaskellGame.HumanInterface.Manager
where

import Graphics.UI.SDL.Events 


pollEvents :: IO (Event) -> [Event] -> IO [Event]  
pollEvents eventAction events = do
  event <- eventAction
  case event of
    NoEvent -> return events
    ev -> pollEvents eventAction (ev:events)

sdlPollEvents :: IO [Event]
sdlPollEvents = pollEvents Graphics.UI.SDL.Events.pollEvent []

