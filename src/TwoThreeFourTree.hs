{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures, FlexibleInstances #-}

module TwoThreeFourTree where

import GHC.Generics (Generic)
import Control.Applicative

data Nat = Z | S Nat

data Node h a
  = T1 (Tree h a) a (Tree h a)
  | T2 (Tree h a) a (Tree h a) a (Tree h a)

data Tree h a where
  Br :: Node h a -> Tree (S n) a
  Lf :: Tree Z a


newtype X f a where
  X :: { unX :: f a } -> X f a
  deriving stock (Show , Eq, Ord)
  deriving newtype (Functor, Applicative, Monad)

instance (Applicative f, Semigroup a) => Semigroup (f a) where
  (<>) = liftA2 (<>)
  