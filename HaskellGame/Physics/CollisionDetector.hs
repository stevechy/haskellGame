module HaskellGame.Physics.CollisionDetector where

import HaskellGame.Types

import Data.List

import Data.IntMap.Lazy

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

detectAndResolveCollisions :: Int -> (GameState, GameEventQueues) -> (GameState, GameEventQueues)
detectAndResolveCollisions delta (gameState, eventQueues) = (Data.List.foldl' respondToCollision gameState (collisions gameState), eventQueues)
                        
respondToCollision :: GameState -> ((Key, t, t1), (Key, t2, t3)) -> GameState                     
respondToCollision gameStateSeed  ((idA,bbA,posA),(idB,bbB, posB)) 
  | movable idA gameStateSeed = gameStateSeed { physicsState = Data.IntMap.Lazy.adjust (\phys -> phys { vy = 0})  idA (physicsState gameStateSeed) }
  | movable idB gameStateSeed = gameStateSeed { physicsState = Data.IntMap.Lazy.adjust (\phys -> phys { vy = 0})  idB (physicsState gameStateSeed) }
  | True = gameStateSeed

movable :: Key -> GameState -> Bool
movable ident gameState = member ident $ actorStates gameState
  
collisions :: GameState -> [(CollisionUnit, CollisionUnit)]
collisions gameState =   
  let collisionsToTest = collisionProduct $ boundingBoxList gameState
  in  Data.List.filter collides collisionsToTest

       
collisionProduct :: [a] -> [(a,a)]
collisionProduct (x:xs) = Data.List.foldr (\ item seed -> (x,item) : seed)  (collisionProduct xs) xs
collisionProduct [] = []
                          

                   