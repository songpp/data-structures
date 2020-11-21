{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module BasicsTest where

import Basics
import Data.List
import Test.SmallCheck.Series
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.SmallCheck as SC

main = defaultMain test_Bascis

test_Bascis = testGroup "QueueTests" [properties, queueUnitTests]

properties :: TestTree
properties = testGroup "Properties" [scProps, queueProperties]

instance (Monad m, Serial m [a]) => Serial m (Queue a)

scProps =
  testGroup
    "(checked by SmallCheck)"
    [ SC.testProperty "sort == sort . reverse" $
        \list -> sort (list :: [Int]) == sort (reverse list),
      SC.testProperty "Fermat's little theorem" $
        \x -> ((x :: Integer) ^ 7 - x) `mod` 7 == 0
      -- the following property does not hold
      -- SC.testProperty "Fermat's last theorem" $
      --   \x y z n ->
      --     (n :: Integer) >= 3 SC.==> x ^ n + y ^ n /= (z ^ n :: Integer)
    ]

queueProperties :: TestTree
queueProperties =
  testGroup
    "Queue Properties"
    [ ]

queueUnitTests :: TestTree
queueUnitTests =
  testGroup
    "Unit tests"
    [ testCase "Empty Queue should be empty" $
        (emptyQueue :: Queue Int) @?= Queue 0 [] [],
      testCase "Enqueue One into EmptyQueue " $
        enqueue 1 emptyQueue @?= Queue 1 [1] [],
      testCase "Dequeue One From singleton queue should be EmptyQueue" $
        dequeue (Queue 1 [1] []) @?= (Just 1, emptyQueue),
      testCase "successive enqueue should be ok" $ 
        (dequeue . enqueue 1 . enqueue 0) emptyQueue @?= (Just 0, Queue 1 [] [1]),
      testCase "Queue Equalities should hold" $ 
        Queue 1 [] [2] @?= Queue 1 [2] [],
      testCase "Queue size" $ 
        (qsize (emptyQueue :: Queue Int) == 0) @? "emptyQueue size is not 0"
    ]