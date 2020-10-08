
{-# LANGUAGE GADTs #-}

module Semi where


import           Data.Semigroup ()
import qualified Data.Semigroup as G


whatIsArg = undefined
  where 
    t :: a -> b -> G.Arg a b
    t = undefined
