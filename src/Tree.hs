{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Tree where

import Control.Concurrent.STM
  ( TMVar,
    TVar,
    atomically,
    newTVarIO,
    readTVar,
    writeTVar,
  )
import Control.Monad (replicateM_)
import Data.Vector.Unboxed (freeze, unsafeFreeze)
import qualified Data.Vector.Unboxed.Mutable as V
  ( replicate,
    unsafeRead,
    write,
  )
import GHC.Generics (Generic)
import System.Random (randomRIO)

randomWrite :: IO ()
randomWrite = do
  vector <- V.replicate 10 (0 :: Int)
  replicateM_ (10 ^ 6) $ do
    i <- randomRIO (0, 9)
    c <- V.unsafeRead vector i
    V.write vector i (c + 1)
  v' <- unsafeFreeze vector
  print v'

class Tree t where
  type S t :: *

data family BplusTree e :: *

data instance BplusTree (Either a b) = Choice a b

createAnotherCounter :: TMVar Int
createAnotherCounter = undefined

makeCounter :: IO (TVar Integer)
makeCounter = newTVarIO 1

incrTVars :: IO ()
incrTVars = do
  c <- makeCounter
  replicateM_ 10 $
    ( atomically $ do
        i <- readTVar c
        writeTVar c (i + 1)
        return i
    )
      >>= print

data BST a where
  Leaf :: a -> BST a
  Node :: {left :: BST a, v :: a, right :: BST a} -> BST a
  deriving (Show, Eq, Generic)
