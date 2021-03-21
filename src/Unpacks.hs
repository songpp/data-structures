{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Unpacks where

import Control.Lens.TH (makeLenses)
import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Unboxed as U
import GHC.Generics (Generic)

data PairP = Pair Int Int deriving (Show)

data PairS = PairS !Int !Int deriving (Show)

data PairU = PairU {-# UNPACK #-} !Int {-# UNPACK #-} !Int deriving (Show)

-- tabnine::sem

data Request a = MakeRequest
  { _id :: String,
    _body :: a
  }
  deriving (Show, Eq, Generic)

$(makeLenses ''Request)

