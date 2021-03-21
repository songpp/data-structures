{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}

module Basics where

import Data.Foldable (foldl')
import qualified Data.List as L
import Data.Map (Map)
import qualified Data.Map as M
import Data.Traversable (forM)
import GHC.Generics (Generic)
import Control.Lens.TH (makeLenses)


sum' :: Int -> [Int] -> Int
sum' acc [] = acc
sum' acc (x : xs) = let !acc' = acc + x in seq acc' (sum' acc' xs)

strictSum :: [Int] -> Int
strictSum = foldl' (+) 0

fibMem :: Int -> Integer
fibMem = (map fibnaci [0 ..] !!)
  where
    fibnaci 0 = 1
    fibnaci 1 = 1
    fibnaci n = fibMem (n - 2) + fibMem (n - 1)

-- map fibnaci [0..] !!
-- (fib 0 : map fib [1..])
-- fib 0 : fib 1 : map ...
-- 1 : 1 : fib 2 : map ..
-- 1 : 1 : (map fib )

twoSum1 :: (Eq a, Num a) => a -> [a] -> Bool
twoSum1 n xs = not $ L.null [True | x <- xs, y <- xs, x /= y && x + y == n]

twoSum2 :: [Int] -> Int -> Bool
twoSum2 xs n = not . L.null $ L.filter (uncurry findTarget) indexed
  where
    indexed = xs `L.zip` [0 ..]
    m = M.fromList indexed
    findTarget x i
      | Just s <- m M.!? (n - x) = s /= i
      | otherwise = False


-- stack
newtype Stack a = Stack [a] deriving (Show, Eq)

isEmpty :: Stack a -> Bool
isEmpty (Stack []) = True
isEmpty _ = False

emptyStack :: Stack a
emptyStack = fromList []

fromList :: [a] -> Stack a
fromList = Stack

push :: a -> Stack a -> Stack a
push a (Stack xs) = Stack (a : xs)

pop :: Stack a -> (Maybe a, Stack a)
pop (Stack (x : xs)) = (Just x, Stack xs)
pop s = (Nothing, s)

data Queue a = Queue
  { _size :: Int,
    _front :: [a],
    _tail :: [a]
  }
  deriving (Show, Generic)

$(makeLenses ''Queue)

qsize :: Queue a -> Int
qsize (Queue s _ _) = s

emptyQueue :: Queue a
emptyQueue = Queue 0 [] []

enqueue :: a -> Queue a -> Queue a
enqueue a (Queue s f t) = Queue (s + 1) (a : f) t

dequeue :: Queue a -> (Maybe a, Queue a)
dequeue q@(Queue 0 _ _) = (Nothing, q)
dequeue (Queue size f (x : xs)) = (Just x, Queue (size - 1) f xs)
dequeue (Queue s f []) = dequeue (Queue s [] (reverse f))

instance Eq a => Eq (Queue a) where
  (Queue s1 fs1 ts1) == (Queue s2 fs2 ts2) = s1 == s2 && fs1 ++ ts1 == fs2 ++ ts2
