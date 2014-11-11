module HaskellGame.Physics.CollisionDetector where

import HaskellGame.Types

import Data.List

import Data.IntMap.Lazy

data SeparatingAxis = XSeparatingAxis | YSeparatingAxis

boundingBoxList :: GameState -> [CollisionUnit]
boundingBoxList gameState = foldrWithKey bbWithPosition [] $ boundingBoxState gameState
  where worldStateItem = worldState gameState
        bbWithPosition key value seed = case Data.IntMap.Lazy.lookup key worldStateItem of
          Just pos -> (key, value, pos) : seed
          Nothing -> seed



collides :: (CollisionUnit, CollisionUnit) -> Bool
collides ((idA,bbA,posA),(idB,bbB,posB)) = ((xa1 <= xb1 && xb1 <= xa2) || (xb1 <= xa1 && xa1 <= xb2)) && ((ya1 <= yb1 && yb1 <= ya2) || (yb1 <= ya1 && ya1 <= yb2))
  where xa1 = (_x posA) +  (relX bbA)
        xa2 = xa1 + (boxWidth bbA)
        xb1 = (_x posB) + (relX bbB)
        xb2 = xb1 + (boxWidth bbB)
        ya1 = (_y posA) + (relY bbA)
        ya2 = ya1 + (boxHeight bbA)
        yb1 = (_y posB) + (relY bbB)
        yb2 = yb1 + (boxHeight bbB)

collidesFast :: (CollisionUnit, CollisionUnit) -> Bool
collidesFast ((_idA,bbA,posA),(_idB,bbB,posB)) = xOverlap && yOverlap
  where
        centerAx = (_x posA) + (relX bbA) + ((boxWidth bbA) `div` 2)
        centerBx = (_x posB) + (relX bbB) + ((boxWidth bbB) `div` 2)
        xOverlap = (abs $ centerAx - centerBx) * 2 < (boxWidth bbA + boxWidth bbB)
        centerAy = (_y posA) + (relY bbA) + ((boxHeight bbA) `div` 2)
        centerBy = (_y posB) + (relY bbB) + ((boxHeight bbB) `div` 2)
        yOverlap = (abs $ centerAy - centerBy) * 2 < (boxHeight bbA + boxHeight bbB)

yOverlapDistance :: (CollisionUnit, CollisionUnit) -> Int
yOverlapDistance ((_idA,bbA,posA),(_idB,bbB,posB))  =
    if distance > 0
        then distance - (boxAHalfHeight + boxBHalfHeight)
        else distance + (boxAHalfHeight + boxBHalfHeight)
    where
            boxAHalfHeight = (boxHeight bbA) `div` 2
            boxBHalfHeight = (boxHeight bbB) `div` 2
            centerAy = (_y posA) + (relY bbA) + (boxAHalfHeight)
            centerBy = (_y posB) + (relY bbB) + (boxBHalfHeight)
            distance = (centerAy - centerBy)

xOverlapDistance :: (CollisionUnit, CollisionUnit) -> Int
xOverlapDistance ((_idA,bbA,posA),(_idB,bbB,posB))  =
    if distance > 0
        then distance - (boxAHalfWidth + boxBHalfWidth)
        else distance + (boxAHalfWidth + boxBHalfWidth)
    where
            boxAHalfWidth = (boxWidth bbA) `div` 2
            boxBHalfWidth = (boxWidth bbB) `div` 2
            centerAy = (_x posA) + (relX bbA) + (boxAHalfWidth)
            centerBy = (_x posB) + (relX bbB) + (boxBHalfWidth)
            distance = (centerAy - centerBy)

detectAndResolveCollisions :: Int -> (GameState, GameEventQueues) -> (GameState, GameEventQueues)
detectAndResolveCollisions _delta (gameState, eventQueues) = (Data.List.foldl' respondToCollision gameState (collisions gameState), eventQueues)

respondToCollision :: GameState -> (CollisionUnit, CollisionUnit) -> GameState
respondToCollision gameStateSeed  ((idA,bbA,posA),(idB,bbB, posB))
  | movable idA gameStateSeed = let (separatingAxis, newPosition) = (adjustOverlap (idA,bbA,posA) (idB,bbB, posB) posA)
                                    in gameStateSeed { physicsState = Data.IntMap.Lazy.adjust (velocityAdjustment separatingAxis) idA (physicsState gameStateSeed),
                                                worldState = Data.IntMap.Lazy.insert idA newPosition (worldState gameStateSeed)
                                              }
  | movable idB gameStateSeed = let (separatingAxis, newPosition) = (adjustOverlap (idB,bbB, posB) (idA,bbA,posA) posB)
                                    in gameStateSeed { physicsState = Data.IntMap.Lazy.adjust (velocityAdjustment separatingAxis)  idB (physicsState gameStateSeed),
                                                worldState = Data.IntMap.Lazy.insert idB newPosition  (worldState gameStateSeed)
                                              }
  | True = gameStateSeed

velocityAdjustment :: SeparatingAxis -> VelocityAcceleration -> VelocityAcceleration
velocityAdjustment separatingAxis =  case separatingAxis of
                                                        XSeparatingAxis -> (\phys -> phys {vx=0})
                                                        YSeparatingAxis -> (\phys -> phys {vy=0})

adjustOverlap :: CollisionUnit -> CollisionUnit -> Position -> (SeparatingAxis,Position)
adjustOverlap (idA,bbA,posA) (idB,bbB, posB) pos = newPosition
    where yOverlap = yOverlapDistance ((idA,bbA,posA),(idB,bbB, posB))
          xOverlap = xOverlapDistance ((idA,bbA,posA),(idB,bbB, posB))
          newPosition = if (abs yOverlap) < (abs xOverlap)
                              then (YSeparatingAxis, pos {_y = (_y pos) - yOverlap })
                              else (XSeparatingAxis, pos {_x = (_x pos) - xOverlap })

movable :: Key -> GameState -> Bool
movable ident gameState = member ident $ actorStates gameState

collisions :: GameState -> [(CollisionUnit, CollisionUnit)]
collisions gameState =
  let collisionsToTest = collisionProduct $ boundingBoxList gameState
  in  Data.List.filter collidesFast collisionsToTest


collisionProduct :: [a] -> [(a,a)]
collisionProduct (x:xs) = Data.List.foldr (\ item seed -> (x,item) : seed)  (collisionProduct xs) xs
collisionProduct [] = []



