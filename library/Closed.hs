module Closed
  ( Endpoint (..)
  , Closed
  , Bounds
  , Single
  , FiniteNat
  , closed
  , unsafeClosed
  , clamp
  , getClosed
  , lowerBound
  , upperBound
  , equals
  , cmp
  , natToClosed
  , weakenUpper
  , weakenLower
  , strengthenUpper
  , strengthenLower
  , add
  , sub
  , multiply
  , isValidClosed
  ) where

import Closed.Internal
