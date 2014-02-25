module HaskellGame.Animation.Animator where

data Clip = FlipbookAnimation Flipbook

data Flipbook = Flipbook { flipBook :: [String]} 

data PlayingClip = PlayingClip { _clip :: Clip , startTime :: Int }

