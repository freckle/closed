{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- Prevent kind errors arising from using * to mean multiplication on
-- type-level natural numbers.
#if __GLASGOW_HASKELL__ >= 806
{-# LANGUAGE NoStarIsType #-}
#endif

{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

module Closed.Internal where

import Control.DeepSeq
import Control.Monad
import Data.Aeson
import qualified Data.Csv as CSV
import Data.Hashable
import Data.Kind (Type)
import Data.Maybe
import Data.Proxy
import Data.Ratio
import Data.Text (pack)
import Database.Persist.Sql
import GHC.Generics
import GHC.Stack
import GHC.TypeLits
import Test.QuickCheck
import Text.ParserCombinators.ReadP (pfail, readP_to_S, readS_to_P)

newtype Closed (a :: Nat) (b :: Nat) = Closed
  { getClosed :: Integer
  }
  deriving (Generic)

-- | Describe whether the endpoint of a 'Bounds' includes
-- or excludes its argument
data Endpoint
  = -- | Endpoint includes its argument
    Inclusive Nat
  | -- | Endpoint excludes its argument
    Exclusive Nat

-- | Syntactic sugar to express open and half-open intervals using
-- the 'Closed' type
type family Bounds (lhs :: Endpoint) (rhs :: Endpoint) :: Type where
  Bounds (Inclusive a) (Inclusive b) = Closed a b
  Bounds (Inclusive a) (Exclusive b) = Closed a (b - 1)
  Bounds (Exclusive a) (Inclusive b) = Closed (a + 1) b
  Bounds (Exclusive a) (Exclusive b) = Closed (a + 1) (b - 1)

-- | Syntactic sugar to express a value that has only one non-bottom
-- inhabitant using the 'Closed' type
type Single (n :: Nat) = Bounds ('Inclusive n) ('Inclusive n)

-- | Syntactic sugar to express a value whose lower bound is zero
type FiniteNat (rhs :: Endpoint) = Bounds ('Inclusive 0) rhs

-- | Proxy for the lower bound of a 'Closed' value
lowerBound :: Closed a b -> Proxy a
lowerBound _ = Proxy

-- | Proxy for the upper bound of a 'Closed' value
upperBound :: Closed a b -> Proxy b
upperBound _ = Proxy

-- | Safely create a 'Closed' value using the specified argument
closed
  :: forall a b
   . (KnownNat a, KnownNat b, a <= b)
  => Integer
  -> Maybe (Closed a b)
closed x = result
 where
  extracted = fromJust result
  result = do
    guard $ x >= natVal (lowerBound extracted) && x <= natVal (upperBound extracted)
    pure $ Closed x

-- | Create a 'Closed' value throwing an error if the argument is not in range
unsafeClosed
  :: forall a b
   . (HasCallStack, KnownNat a, KnownNat b, a <= b)
  => Integer
  -> Closed a b
unsafeClosed x = result
 where
  result =
    if x >= natVal (lowerBound result) && x <= natVal (upperBound result)
      then Closed x
      else error $ unrepresentable x result "unsafeClosed"

-- | Clamp an @'Integral'@ in the range constrained by a @'Closed'@ interval
clamp
  :: forall a b i
   . (Integral i, KnownNat a, KnownNat b, a <= b)
  => i
  -> Closed a b
clamp x
  | fromIntegral x < getClosed (minBound @(Closed a b)) = minBound
  | fromIntegral x > getClosed (maxBound @(Closed a b)) = maxBound
  | otherwise = Closed (fromIntegral x)

-- | Test equality on 'Closed' values in the same range
instance Eq (Closed a b) where
  Closed x == Closed y = x == y

-- | Compare 'Closed' values in the same range
instance Ord (Closed a b) where
  Closed x `compare` Closed y = x `compare` y

-- | Generate the lowest and highest inhabitant of a given 'Closed' type
instance (KnownNat a, KnownNat b, a <= b) => Bounded (Closed a b) where
  maxBound = result
   where
    result = Closed (natVal (upperBound result))

  minBound = result
   where
    result = Closed (natVal (lowerBound result))

-- | Enumerate values in the range of a given 'Closed' type
instance (KnownNat a, KnownNat b, a <= b) => Enum (Closed a b) where
  fromEnum = fromEnum . getClosed
  toEnum = unsafeClosed . toEnum
  enumFrom x = enumFromTo x maxBound
  enumFromThen x y = enumFromThenTo x y (if x >= y then minBound else maxBound)

instance Show (Closed a b) where
  showsPrec d (Closed x) = showParen (d > 9) $ showString "unsafeClosed " . showsPrec 10 x

instance (KnownNat a, KnownNat b, a <= b) => Read (Closed a b) where
  readsPrec n = readP_to_S $ do
    i <- readS_to_P $ readsPrec @Integer n
    maybe pfail pure $ closed @a @b i

-- | Bounded arithmetic, e.g. maxBound + 1 == maxBound
instance (KnownNat a, KnownNat b, a <= b) => Num (Closed a b) where
  Closed x + Closed y = Closed $ min (x + y) (fromIntegral (maxBound :: Closed a b))
  Closed x - Closed y = Closed $ max (x - y) (fromIntegral (minBound :: Closed a b))
  Closed x * Closed y = Closed $ min (x * y) (fromIntegral (maxBound :: Closed a b))
  abs = id
  signum = const 1
  fromInteger x = result
   where
    result =
      if x >= natVal (lowerBound result) && x <= natVal (upperBound result)
        then Closed x
        else error $ unrepresentable x result "fromInteger"

instance (KnownNat a, KnownNat b, a <= b) => Real (Closed a b) where
  toRational (Closed x) = x % 1

instance (KnownNat a, KnownNat b, a <= b) => Integral (Closed a b) where
  quotRem (Closed x) (Closed y) = (Closed $ x `quot` y, Closed $ x `rem` y)
  toInteger (Closed x) = x

instance NFData (Closed a b)

instance Hashable (Closed a b)

instance ToJSON (Closed a b) where
  toEncoding = toEncoding . getClosed
  toJSON = toJSON . getClosed

instance (KnownNat a, KnownNat b, a <= b) => FromJSON (Closed a b) where
  parseJSON v = do
    x <- parseJSON v
    case closed x of
      Just cx -> pure cx
      n -> fail $ unrepresentable x (fromJust n) "parseJSON"

instance CSV.ToField (Closed a b) where
  toField = CSV.toField . getClosed

instance (KnownNat a, KnownNat b, a <= b) => CSV.FromField (Closed a b) where
  parseField s = do
    x <- CSV.parseField s
    case closed x of
      Just cx -> pure cx
      n -> fail $ unrepresentable x (fromJust n) "parseField"

instance (KnownNat a, KnownNat b, a <= b) => Arbitrary (Closed a b) where
  arbitrary =
    Closed <$> choose (natVal @a Proxy, natVal @b Proxy)

instance (KnownNat a, KnownNat b, a <= b) => PersistField (Closed a b) where
  toPersistValue = toPersistValue . fromIntegral @Integer @Int . getClosed
  fromPersistValue value = do
    x <- fromIntegral @Int @Integer <$> fromPersistValue value
    case closed @a @b x of
      Just cx -> pure cx
      n -> Left $ pack $ unrepresentable x (fromJust n) "fromPersistValue"

instance (KnownNat a, KnownNat b, a <= b) => PersistFieldSql (Closed a b) where
  sqlType _ = sqlType (Proxy @Int)

unrepresentable
  :: (KnownNat a, KnownNat b)
  => Integer
  -> Closed a b
  -> String
  -> String
unrepresentable x cx prefix =
  prefix
    <> ": Integer "
    <> show x
    <> " is not representable in Closed "
    <> show (natVal $ lowerBound cx)
    <> " "
    <> show (natVal $ upperBound cx)

-- | Convert a type-level literal into a 'Closed' value
natToClosed
  :: forall a b x proxy
   . (KnownNat a, KnownNat b, KnownNat x, a <= x, x <= b)
  => proxy x
  -> Closed a b
natToClosed p = Closed $ natVal p

-- | Add inhabitants at the end
weakenUpper :: forall k a b. (a <= b, b <= k) => Closed a b -> Closed a k
weakenUpper (Closed x) = Closed x

-- | Add inhabitants at the beginning
weakenLower :: forall k a b. (a <= b, k <= a) => Closed a b -> Closed k b
weakenLower (Closed x) = Closed x

-- | Remove inhabitants from the end. Returns 'Nothing' if the input was removed
strengthenUpper
  :: forall k a b
   . (KnownNat a, KnownNat b, KnownNat k, a <= b, a <= k, k <= b)
  => Closed a b
  -> Maybe (Closed a k)
strengthenUpper (Closed x) = result
 where
  result = do
    guard $ x <= natVal (upperBound $ fromJust result)
    pure $ Closed x

-- | Remove inhabitants from the beginning. Returns 'Nothing' if the input was removed
strengthenLower
  :: forall k a b
   . (KnownNat a, KnownNat b, KnownNat k, a <= b, a <= k, k <= b)
  => Closed a b
  -> Maybe (Closed k b)
strengthenLower (Closed x) = result
 where
  result = do
    guard $ x >= natVal (lowerBound $ fromJust result)
    pure $ Closed x

-- | Test two different types of 'Closed' values for equality.
equals :: Closed a b -> Closed o p -> Bool
equals (Closed x) (Closed y) = x == y

infix 4 `equals`

-- | Compare two different types of 'Closed' values
cmp :: Closed a b -> Closed o p -> Ordering
cmp (Closed x) (Closed y) = x `compare` y

-- | Add two different types of 'Closed' values
add :: Closed a b -> Closed o p -> Closed (n + o) (m + p)
add (Closed x) (Closed y) = Closed $ x + y

-- | Subtract two different types of 'Closed' values
-- Returns 'Left' for negative results, and 'Right' for positive results.
sub
  :: Closed a b
  -> Closed o p
  -> Either (Closed (o - n) (p - m)) (Closed (n - o) (m - p))
sub (Closed x) (Closed y)
  | x >= y = Right $ Closed $ x - y
  | otherwise = Left $ Closed $ y - x

-- | Multiply two different types of 'Closed' values
multiply :: Closed a b -> Closed o p -> Closed (a * o) (b * p)
multiply (Closed x) (Closed y) = Closed $ x * y

-- | Verifies that a given 'Closed' value is valid.
-- Should always return 'True' unles you bring the @Closed.Internal.Closed@ constructor into scope,
-- or use 'Unsafe.Coerce.unsafeCoerce' or other nasty hacks
isValidClosed :: (KnownNat a, KnownNat b) => Closed a b -> Bool
isValidClosed cx@(Closed x) =
  natVal (lowerBound cx) <= x && x <= natVal (upperBound cx)
