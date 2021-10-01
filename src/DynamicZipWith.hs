{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{-
Inferring the arity of zipWith, with lots of type-level hackery
https://www.youtube.com/watch?v=iGSKqcebhfs
-}
module DynamicZipWith where

import Data.Kind (Constraint, Type)
import Prelude hiding (zipWith)

data Nat = Succ Nat | Zero

type CountArgs :: Type -> Nat
type family CountArgs f where
  CountArgs (a -> b) = Succ (CountArgs b)
  CountArgs others = Zero

type ListsFrom :: Type -> Type
type family ListsFrom f where
  ListsFrom (a -> b) = [a] -> ListsFrom b
  ListsFrom others = [others]

type ListsFromWitness :: Type -> Nat -> Type
data ListsFromWitness f n where
  ListsFromFunc :: LFC b => ListsFromWitness (a -> b) (Succ n)
  ListsFromNil :: forall others. ListsFrom others ~ [others] => ListsFromWitness others Zero

type LFC f = ListsFromClass f (CountArgs f)

type ListsFromClass :: Type -> Nat -> Constraint
class ListsFromClass f n where
  witness :: ListsFromWitness f n

instance (ListsFromClass b n, CountArgs b ~ n) => ListsFromClass (a -> b) (Succ n) where
  witness = ListsFromFunc

instance ListsFrom o ~ [o] => ListsFromClass o Zero where
  witness = ListsFromNil

zipWith :: forall f. LFC f => f -> ListsFrom f
zipWith f = go (repeat f)
  where
    go :: forall local_f. LFC local_f => [local_f] -> ListsFrom local_f
    go funcs = case witness @local_f @(CountArgs local_f) of
      ListsFromNil -> funcs
      ListsFromFunc -> go . apply funcs

    apply :: forall a b. [a -> b] -> [a] -> [b]
    apply (f : fs) (x : xs) = f x : apply fs xs
    apply _ _ = []

f1 _ _ _ = 3