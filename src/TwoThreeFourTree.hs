{-# LANGUAGE GADTs, DataKinds, KindSignatures #-}
module TwoThreeFourTree where

data Nat = Z | S Nat

data Node h a = 
    T1 (Tree h a) a (Tree h a)
  | T2 (Tree h a) a (Tree h a) a (Tree h a)


data Tree h a where
  Br :: Node h a -> Tree (S n) a
  Lf :: Tree Z a