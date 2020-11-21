{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Hanoi where

import Control.Monad (foldM, foldM_)
import Data.Foldable (foldl')
import Text.Printf ()

data State = State
  { id :: String
  , name :: String
  } deriving (Show, Eq, Ord)

lists = [1 .. 100]

class Solvable a where
  solve :: State -> a
