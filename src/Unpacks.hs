{-# LANGUAGE TypeFamilies, TypeSynonymInstances #-}

module Unpacks where 

import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Unboxed         as U

data PairP = Pair Int Int deriving Show
data PairS = PairS !Int !Int deriving Show
data PairU = PairU {-# UNPACK #-} !Int {-# UNPACK #-} !Int deriving Show

