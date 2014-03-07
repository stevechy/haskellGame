module Main

where

import Test.HUnit

tests :: Test
tests = TestList [
    TestLabel "TestLabel"
        (TestCase 
            (do  putStrLn "hi"
                 assertBool "Test" True)
        )
    ]


main :: IO ()
main = do _ <- runTestTT tests
          return ()
