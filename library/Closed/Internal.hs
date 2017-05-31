{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}

module Closed.Internal
  ( Closed(..)
  ) where

import GHC.TypeLits
import GHC.Generics

newtype Closed (n :: Nat) (m :: Nat) = Closed { getClosed :: Integer }
  deriving (Generic)
