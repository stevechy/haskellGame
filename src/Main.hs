module Main where

import qualified Graphics.UI.SDL
import Graphics.UI.SDL.Events
import Graphics.UI.SDL.Time
import Graphics.UI.SDL.Video
import Graphics.UI.SDL.TTF
import GHC.Word
import Data.List
import qualified Data.Maybe
import Data.IntMap.Lazy

import HaskellGame.Types
import HaskellGame.Game

import qualified HaskellGame.Physics.PhysicsSimulator as PhysicsSimulator
import qualified HaskellGame.Physics.CollisionDetector as CollisionDetector
import qualified HaskellGame.Rendering.Renderer as Renderer
import qualified HaskellGame.Gameplay.GamePlaySimulator as GamePlaySimulator
import qualified HaskellGame.Resources.ResourceManager
import qualified HaskellGame.HumanInterface.HumanInterfaceManager as HumanInterfaceManager
import qualified HaskellGame.Menu.Manager
import qualified HaskellGame.Animation.Animator as Animator
import qualified HaskQuery

main :: IO ()
main = Graphics.UI.SDL.withInit [Graphics.UI.SDL.InitEverything] runGame

playerId :: Int
playerId = 1

floorId :: Int
floorId = 2

platformId :: Int
platformId = 3


walkCycleId :: Int
walkCycleId = 4

jumpId :: Int
jumpId = 5

player2Id :: Int
player2Id = 6

floor2Id :: Int
floor2Id = 7

cameraId :: Int
cameraId = 8

randomSquareId :: Int
randomSquareId = 99

runGame :: IO ()
runGame = do
  _ <- Graphics.UI.SDL.TTF.init
  font <- openFont "Fonts/SourceSansPro-Black.ttf" 20
  Just videoSurface <- Graphics.UI.SDL.Video.trySetVideoMode 800 540 32 [ Graphics.UI.SDL.DoubleBuf]
  resources <- HaskellGame.Resources.ResourceManager.loadResources playerId walkCycleId jumpId

  let menuState = MenuState { menuPosition = 0, menuItems = ["Start Game","Options","Quit"], menuFont = font }
  HaskellGame.Menu.Manager.runMenu menuState videoSurface

  initialTicks <- Graphics.UI.SDL.Time.getTicks

  let gameState = initializeGameState $ emptyGameState { worldState = Data.IntMap.Lazy.empty,
                              _resources = resources,
                              actorStates = Data.IntMap.Lazy.empty,
                              physicsState = Data.IntMap.Lazy.empty,
                              _animationStates = Data.IntMap.Lazy.empty,
                              boundingBoxState = Data.IntMap.Lazy.empty,
                              renderingHandlers = Data.IntMap.Lazy.empty,
                              _font = Just font,
                              _currentGameTime = 0}


  let eventAction = Graphics.UI.SDL.Events.pollEvent
  let drawAction = Renderer.drawGame videoSurface


  gameLoop drawAction eventAction gameState initialTicks

  return ()

initializeGameState :: GameState -> GameState
initializeGameState gameState =
    insertEntities gameState [GameEntity randomSquareId [toComponent (BoundingBox 0 0 10 10),
                                              toComponent (Position 300 5),
                                              toComponent Renderer.rectRenderer ],
                              GameEntity playerId [toComponent $ Position 5 5,
                                                    toComponent $ VelocityAcceleration {vx = 0, vy = 0.00, ax = 0, ay = 0.0002},
                                                    toComponent $ BoundingBox 0 0 66 92,
                                                    toComponent $ Idle,
                                                    toComponent $ Renderer.animatedRender,
                                                    toComponent $ AnimationClip {_resourceId = playerId, _startTime = 0, _rate = 125},
                                                    toComponent $ Player { _playerId = 1, _playerObjectIdentifier = playerId}
                                                    ],
                              GameEntity player2Id [toComponent $ Position 50 5,
                                                    toComponent $ VelocityAcceleration {vx = 0, vy = 0.00, ax = 0, ay = 0.0002},
                                                    toComponent $ BoundingBox 0 0 66 92,
                                                    toComponent $ Idle,
                                                    toComponent $ Renderer.animatedRender,
                                                    toComponent $ AnimationClip {_resourceId = playerId, _startTime = 0, _rate = 125},
                                                    toComponent $ Player { _playerId = 2, _playerObjectIdentifier = player2Id}
                                                    ],
                              GameEntity floorId  [toComponent $ Position 0 400,
                                                     toComponent $ Renderer.rectRenderer,
                                                     toComponent $ BoundingBox 0 0 640 10
                                                    ],
                              GameEntity floor2Id  [toComponent $ Position 700 400,
                                                     toComponent $ Renderer.rectRenderer,
                                                     toComponent $ BoundingBox 0 0 640 50
                                                    ],
                              GameEntity platformId  [toComponent $ Position 500 200,
                                                     toComponent $ Renderer.rectRenderer,
                                                     toComponent $ BoundingBox (-25) (-25) 50 50
                                                    ],
                              GameEntity cameraId   [ toComponent $ Camera { _cameraId = 1, _cameraEntityId=cameraId},
                                                      toComponent $ Position { _x = 100, _y = 0}
                                                    ]
                             ]




gameLoop :: (GameState -> IO t) -> IO Event -> GameState -> GHC.Word.Word32 -> IO ()
gameLoop drawAction eventAction gameState lastFrameTicks = do

  events <- HumanInterfaceManager.pollEvents eventAction []
  let gameEvents = emptyGameEventQueues { gameActions = concat $ Data.List.map (HumanInterfaceManager.playerGameAction gameState) events,
                                  physicsActions = [] }

  let state = Data.Maybe.isNothing $ find (\x -> x == Graphics.UI.SDL.Events.Quit) events

  currentTicks <- Graphics.UI.SDL.Time.getTicks

  let frameDelay = fromIntegral $ currentTicks - lastFrameTicks

  let (finalState, _finalQueues) =
        Data.List.foldl' ( \ currentGameState gameStep -> gameStep currentGameState) (gameState, gameEvents)
                   [ (\ (currentGameState, currentQueues) -> (currentGameState {_currentGameTime = fromIntegral $ currentTicks }, currentQueues)),
                     GamePlaySimulator.processGameStateOutputEvents,
                     PhysicsSimulator.applyPhysicsChanges,
                     PhysicsSimulator.applyPhysics frameDelay,
                     CollisionDetector.detectAndResolveCollisions frameDelay,
                     Animator.updateAnimationState
                   ]


  _ <- drawAction finalState

  case state of
    True -> do
      Graphics.UI.SDL.Time.delay 30
      gameLoop drawAction eventAction finalState currentTicks
    False -> return ()



