{-# LANGUAGE GADTs #-}

module Basics where

import qualified Data.List as L
import Data.Map (Map)
import qualified Data.Map as M

sum1 :: [Int] -> Int
sum1 [] = 0
sum1 (x : xs) = x + sum1 xs

sum' :: Int -> [Int] -> Int
sum' acc [] = acc
sum' acc (x : xs) = let acc' = acc + x in seq acc' (sum' acc' xs)

fib_mem :: Int -> Integer
fib_mem = (map fibnaci [0 ..] !!)
  where
    fibnaci 0 = 1
    fibnaci 1 = 1
    fibnaci n = fib_mem (n - 2) + fib_mem (n - 1)

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