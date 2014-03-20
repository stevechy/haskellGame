module HaskellGame.Animation.Animator where


import HaskellGame.Types
import qualified Data.IntMap.Lazy

updateAnimationState ::  (GameState, GameEventQueues) -> (GameState, GameEventQueues)
updateAnimationState (gameState, gameEventQueues) = (updatedGameState, gameEventQueues)
    where updatedGameState = foldr (processAnimationAction) gameState (animationActions gameEventQueues)


processAnimationAction :: GameEvent AnimationAction -> GameState -> GameState
processAnimationAction animationAction gameState = let animationStates = _animationStates gameState in
    case animationAction of
        GameEvent { _identifier = identifier ,  _gameEvent = StartClip {_clip = clip} } -> gameState {_animationStates = Data.IntMap.Lazy.insert identifier (clip { _startTime = _currentGameTime gameState}) animationStates}


