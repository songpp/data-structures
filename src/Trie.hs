{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Trie where

import Data.Foldable (Foldable (toList, foldr))
import GHC.Generics (Generic)

-- import Data.Traversable.TreeLike

data BST a where
  Leaf :: a -> BST a
  Node :: {left :: BST a, value :: a, right :: BST a} -> BST a
  deriving (Show, Eq, Generic)

mkTrie :: a -> BST a
mkTrie = Leaf

data SkewHeap a where
  SEmpty :: SkewHeap a
  SNode :: SkewHeap a -> a -> SkewHeap a -> SkewHeap a
  deriving (Show, Eq)
  deriving stock (Functor, Foldable, Generic)

mkSkewHep :: a -> SkewHeap a
mkSkewHep v = SNode SEmpty v SEmpty

pop :: Ord a => SkewHeap a -> (Maybe a, SkewHeap a)
pop SEmpty = (Nothing, SEmpty)
pop (SNode l a r) = (Just a, mergeSH l r)

popAll :: Ord a => SkewHeap a -> [a]
popAll s = drain (pop s)
  where
    drain (Just x, n) = x : popAll n
    drain (Nothing, _) = []

walk :: SkewHeap a -> [a]
walk SEmpty = []
walk (SNode l v r) = walk l ++ [v] ++ walk r

mergeSH :: Ord a => SkewHeap a -> SkewHeap a -> SkewHeap a
mergeSH = go
  where
    go SEmpty r = r
    go l SEmpty = l
    go a@(SNode l x r) b@(SNode l' y r')
      | x <= y = SNode l x (mergeSH b r)
      | otherwise = SNode l' y (mergeSH a r')

newtype PriorityQueue a = PQ (SkewHeap a) deriving (Show, Eq)

emptyPQ :: PriorityQueue a
emptyPQ = PQ SEmpty

insertPQ :: Ord a => a -> PriorityQueue a -> PriorityQueue a
insertPQ x (PQ h) = PQ (mergeSH h (mkSkewHep x))

popPQ :: Ord a => PriorityQueue a -> (Maybe a, PriorityQueue a)
popPQ (PQ h) = (res, PQ h') where (res, h') = pop h

sizePQ :: PriorityQueue a -> Int
sizePQ (PQ h) = length (toList h) 

-- instance TreeLike SkewHeap where
--   treeTraverse _ _ SEmpty = pure SEmpty
--   treeTraverse f g (SNode l v r) = SNode <$> g l <*> f v  <*> g r
