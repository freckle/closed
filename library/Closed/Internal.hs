{-# LANGUAGE CPP                  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

-- Prevent kind errors arising from using * to mean multiplication on
-- type-level natural numbers.
#if __GLASGOW_HASKELL__ >= 806
{-# LANGUAGE NoStarIsType         #-}
#endif

{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

module Closed.Internal where

import           Control.DeepSeq
import           Control.Monad
import           Data.Aeson
import qualified Data.Csv             as CSV
import           Data.Hashable
import           Data.Kind            (Type)
import           Data.Maybe
import           Data.Proxy
import           Data.Ratio
import           Data.Text            (pack)
import           Database.Persist.Sql
import           GHC.Generics
import           GHC.Stack
import           GHC.TypeLits
import           Test.QuickCheck

newtype Closed (n :: Nat) (m :: Nat)
  = Closed { getClosed :: Integer }
  deriving (Generic)

-- | Describe whether the endpoint of a 'Bounds' includes
-- or excludes its argument
data Endpoint
  -- | Endpoint includes its argument
  = Inclusive Nat
  -- | Endpoint excludes its argument
  | Exclusive Nat

-- | Syntactic sugar to express open and half-open intervals using
-- the 'Closed' type
type family Bounds (lhs :: Endpoint) (rhs :: Endpoint) :: Type where
  Bounds (Inclusive n) (Inclusive m) = Closed  n       m
  Bounds (Inclusive n) (Exclusive m) = Closed  n      (m - 1)
  Bounds (Exclusive n) (Inclusive m) = Closed (n + 1)  m
  Bounds (Exclusive n) (Exclusive m) = Closed (n + 1) (m - 1)

-- | Syntactic sugar to express a value that has only one non-bottom
-- inhabitant using the 'Closed' type
type Single (n :: Nat) = Bounds ('Inclusive n) ('Inclusive n)

-- | Syntactic sugar to express a value whose lower bound is zero
type FiniteNat (rhs :: Endpoint) = Bounds ('Inclusive 0) rhs

-- | Proxy for the lower bound of a 'Closed' value
lowerBound :: Closed n m -> Proxy n
lowerBound _ = Proxy

-- | Proxy for the upper bound of a 'Closed' value
upperBound :: Closed n m -> Proxy m
upperBound _ = Proxy

-- | Safely create a 'Closed' value using the specified argument
closed :: forall n m. (n <= m, KnownNat n, KnownNat m) => Integer -> Maybe (Closed n m)
closed x = result
 where
  extracted = fromJust result
  result = do
    guard $ x >= natVal (lowerBound extracted) && x <= natVal (upperBound extracted)
    pure $ Closed x

-- | Create a 'Closed' value throwing an error if the argument is not in range
unsafeClosed :: forall n m. (HasCallStack, n <= m, KnownNat n, KnownNat m) => Integer -> Closed n m
unsafeClosed x = result
 where
  result =
    if x >= natVal (lowerBound result) && x <= natVal (upperBound result)
      then Closed x
      else error $ unrepresentable x result "unsafeClosed"

-- | Clamp an @'Integral'@ in the range constrained by a @'Closed'@ interval
clamp :: forall n m a. (KnownNat n, KnownNat m, n <= m, Integral a) => a -> Closed n m
clamp x
  | fromIntegral x < getClosed (minBound @(Closed n m)) = minBound
  | fromIntegral x > getClosed (maxBound @(Closed n m)) = maxBound
  | otherwise = Closed (fromIntegral x)

-- | Test equality on 'Closed' values in the same range
instance Eq (Closed n m) where
  Closed x == Closed y = x == y

-- | Compare 'Closed' values in the same range
instance Ord (Closed n m) where
  Closed x `compare` Closed y = x `compare` y

-- | Generate the lowest and highest inhabitant of a given 'Closed' type
instance (n <= m, KnownNat n, KnownNat m) => Bounded (Closed n m) where
  maxBound = result
   where
    result = Closed (natVal (upperBound result))

  minBound = result
   where
    result = Closed (natVal (lowerBound result))

-- | Enumerate values in the range of a given 'Closed' type
instance (n <= m, KnownNat n, KnownNat m) => Enum (Closed n m) where
  fromEnum = fromEnum . getClosed
  toEnum = unsafeClosed . toEnum
  enumFrom x = enumFromTo x maxBound
  enumFromThen x y = enumFromThenTo x y (if x >= y then minBound else maxBound)

instance Show (Closed n m) where
  showsPrec d (Closed x) = showParen (d > 9) $ showString "unsafeClosed " . showsPrec 10 x

-- | Bounded arithmetic, e.g. maxBound + 1 == maxBound
instance (n <= m, KnownNat n, KnownNat m) => Num (Closed n m) where
  Closed x + Closed y = Closed $ min (x + y) (fromIntegral (maxBound :: Closed n m))
  Closed x - Closed y = Closed $ max (x - y) (fromIntegral (minBound :: Closed n m))
  Closed x * Closed y = Closed $ min (x * y) (fromIntegral (maxBound :: Closed n m))
  abs = id
  signum = const 1
  fromInteger x = result
   where
    result =
      if x >= natVal (lowerBound result) && x <= natVal (upperBound result)
        then Closed x
        else error $ unrepresentable x result "fromInteger"

instance (n <= m, KnownNat n, KnownNat m) => Real (Closed n m) where
  toRational (Closed x) = x % 1

instance (n <= m, KnownNat n, KnownNat m) => Integral (Closed n m) where
  quotRem (Closed x) (Closed y) = (Closed $ x `quot` y, Closed $ x `rem` y)
  toInteger (Closed x) = x

instance NFData (Closed n m)

instance Hashable (Closed n m)

instance ToJSON (Closed n m) where
  toEncoding = toEncoding . getClosed
  toJSON = toJSON . getClosed

instance (n <= m, KnownNat n, KnownNat m) => FromJSON (Closed n m) where
  parseJSON v = do
    x <- parseJSON v
    case closed x of
      Just cx -> pure cx
      n       -> fail $ unrepresentable x (fromJust n) "parseJSON"

instance CSV.ToField (Closed n m) where
  toField = CSV.toField . getClosed

instance (n <= m, KnownNat n, KnownNat m) => CSV.FromField (Closed n m) where
  parseField s = do
    x <- CSV.parseField s
    case closed x of
      Just cx -> pure cx
      n       -> fail $ unrepresentable x (fromJust n) "parseField"

instance (n <= m, KnownNat n, KnownNat m) => Arbitrary (Closed n m) where
  arbitrary =
    Closed <$> choose (natVal @n Proxy, natVal @m Proxy)

instance (n <= m, KnownNat n, KnownNat m) => PersistField (Closed n m) where
  toPersistValue = toPersistValue . fromIntegral @Integer @Int . getClosed
  fromPersistValue value = do
    x <- fromIntegral @Int @Integer <$> fromPersistValue value
    case closed @n @m x of
      Just cx -> pure cx
      n       -> Left $ pack $ unrepresentable x (fromJust n) "fromPersistValue"

instance (n <= m, KnownNat n, KnownNat m) => PersistFieldSql (Closed n m) where
  sqlType _ = sqlType (Proxy @Int)

unrepresentable :: (KnownNat n, KnownNat m) => Integer -> Closed n m -> String -> String
unrepresentable x cx prefix =
  prefix ++ ": Integer " ++ show x ++
  " is not representable in Closed " ++ show (natVal $ lowerBound cx) ++
  " " ++ show (natVal $ upperBound cx)

-- | Convert a type-level literal into a 'Closed' value
natToClosed :: forall n m x proxy. (n <= x, x <= m, KnownNat x, KnownNat n, KnownNat m) => proxy x -> Closed n m
natToClosed p = Closed $ natVal p

-- | Add inhabitants at the end
weakenUpper :: forall k n m. (n <= m, m <= k) => Closed n m -> Closed n k
weakenUpper (Closed x) = Closed x

-- | Add inhabitants at the beginning
weakenLower :: forall k n m. (n <= m, k <= n) => Closed n m -> Closed k m
weakenLower (Closed x) = Closed x

-- | Remove inhabitants from the end. Returns 'Nothing' if the input was removed
strengthenUpper :: forall k n m. (KnownNat n, KnownNat m, KnownNat k, n <= m, n <= k, k <= m) => Closed n m -> Maybe (Closed n k)
strengthenUpper (Closed x) = result
 where
  result = do
    guard $ x <= natVal (upperBound $ fromJust result)
    pure $ Closed x

-- | Remove inhabitants from the beginning. Returns 'Nothing' if the input was removed
strengthenLower :: forall k n m. (KnownNat n, KnownNat m, KnownNat k, n <= m, n <= k, k <= m) => Closed n m -> Maybe (Closed k m)
strengthenLower (Closed x) = result
 where
  result = do
    guard $ x >= natVal (lowerBound $ fromJust result)
    pure $ Closed x

-- | Test two different types of 'Closed' values for equality.
equals :: Closed n m -> Closed o p -> Bool
equals (Closed x) (Closed y) = x == y
infix 4 `equals`

-- | Compare two different types of 'Closed' values
cmp :: Closed n m -> Closed o p -> Ordering
cmp (Closed x) (Closed y) = x `compare` y

-- | Add two different types of 'Closed' values
add :: Closed n m -> Closed o p -> Closed (n + o) (m + p)
add (Closed x) (Closed y) = Closed $ x + y

-- | Subtract two different types of 'Closed' values
-- Returns 'Left' for negative results, and 'Right' for positive results.
sub :: Closed n m -> Closed o p -> Either (Closed (o - n) (p - m)) (Closed (n - o) (m - p))
sub (Closed x) (Closed y)
  | x >= y = Right $ Closed $ x - y
  | otherwise = Left $ Closed $ y - x

-- | Multiply two different types of 'Closed' values
multiply :: Closed n m -> Closed o p -> Closed (n * o) (m * p)
multiply (Closed x) (Closed y) = Closed $ x * y

-- | Verifies that a given 'Closed' value is valid.
-- Should always return 'True' unles you bring the @Closed.Internal.Closed@ constructor into scope,
-- or use 'Unsafe.Coerce.unsafeCoerce' or other nasty hacks
isValidClosed :: (KnownNat n, KnownNat m) => Closed n m -> Bool
isValidClosed cx@(Closed x) =
  natVal (lowerBound cx) <= x && x <= natVal (upperBound cx)
