{-# LANGUAGE DataKinds, KindSignatures #-}
module Closed.Internal
  ( Closed(..)
  ) where

import GHC.TypeLits

newtype Closed (n :: Nat) (m :: Nat) = Closed { getClosed :: Integer }
