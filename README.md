# closed

Integers bounded by a closed interval

## Build

```plaintext
stack build
```

## Tutorial

### Overview

This package exports one core data type `Closed (n :: Nat) (m :: Nat)` for describing integers bounded by a closed interval. That is, given `cx :: Closed n m`, `getClosed cx` is an integer `x` where `n <= x <= m`.

We also export a type family `Bounds` for describing open and half-open intervals in terms of closed intervals.

```plaintext
Bounds (Inclusive 0) (Inclusive 10) => Closed 0 10
Bounds (Inclusive 0) (Exclusive 10) => Closed 0 9
Bounds (Exclusive 0) (Inclusive 10) => Closed 1 10
Bounds (Exclusive 0) (Exclusive 10) => Closed 1 9
```

### Preamble

For most uses of `closed`, you'll only need `DataKinds` and maybe `TypeFamilies`. The other extensions below just make some of the tests concise.

```haskell
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

module Main where

import Closed
import Control.Exception
import Data.Aeson
import Database.Persist
import Data.Proxy
import Data.Text
import Data.Vector
import GHC.TypeLits
import qualified Data.Csv as CSV
import Test.Hspec
import Test.Hspec.QuickCheck

main :: IO ()
main = hspec $ do
```

### Construction

The safe constructor `closed` uses `Maybe` to indicate failure. There is also an unsafe constructor `unsafeClosed` as well as a `Num` instance that implements `fromInteger`.

```haskell
  describe "safe construction" $ do

    it "should successfully construct values in the specified bounds" $ do
      let result = closed 2 :: Maybe (Bounds (Inclusive 2) (Exclusive 5))
      getClosed <$> result `shouldBe` Just 2

    it "should fail to construct values outside the specified bounds" $ do
      let result = closed 1 :: Maybe (Bounds (Inclusive 2) (Exclusive 5))
      getClosed <$> result `shouldBe` Nothing

  describe "unsafe construction" $ do

    it "should successfully construct values in the specified bounds" $ do
      -- Note that you can use -XTypeApplications instead of type annotations
      let result = unsafeClosed @2 @4 2
      getClosed result `shouldBe` 2

    it "should fail to construct values outside the specified bounds" $ do
      let result = unsafeClosed @2 @4 1
      evaluate (getClosed result) `shouldThrow` anyErrorCall

  describe "unsafe literal construction" $ do

    it "should successfully construct values in the specified bounds" $ do
      let result = 2 :: Bounds (Inclusive 2) (Exclusive 5)
      getClosed result `shouldBe` 2

    it "should fail to construct values outside the specified bounds" $ do
      let result = 1 :: Bounds (Inclusive 2) (Exclusive 5)
      evaluate (getClosed result) `shouldThrow` anyErrorCall
```

### Elimination

Use `getClosed` to extract the `Integer` from a `Closed` value.

```haskell
  describe "elimination" $ do

    it "should allow the integer value to be extracted" $ do
      let result = 1 :: Bounds (Inclusive 0) (Exclusive 10)
      getClosed result `shouldBe` 1
```

### Bounds Manipulation

The upper and lower bounds can be queried, strengthened, and weakened.

```haskell
  describe "bounds manipulation" $ do

    let cx = 4 :: Bounds (Inclusive 2) (Exclusive 10)

    it "should allow querying the bounds" $ do
      upperBound cx `shouldBe` (Proxy @9)
      lowerBound cx `shouldBe` (Proxy @2)

    it "should allow weakening the bounds" $ do
      upperBound (weakenUpper cx) `shouldBe` (Proxy @10)
      lowerBound (weakenLower cx) `shouldBe` (Proxy @1)

    it "should allow weakening the bounds by more than one" $ do
      upperBound (weakenUpper cx) `shouldBe` (Proxy @20)
      lowerBound (weakenLower cx) `shouldBe` (Proxy @0)

    it "should allow strengthening the bounds" $ do
      upperBound <$> strengthenUpper cx `shouldBe` Just (Proxy @8)
      lowerBound <$> strengthenLower cx `shouldBe` Just (Proxy @3)

    it "should allow strengthening the bounds by more than one" $ do
      upperBound <$> strengthenUpper cx `shouldBe` Just (Proxy @7)
      lowerBound <$> strengthenLower cx `shouldBe` Just (Proxy @4)
```

### Arithmetic

Arithmetic gets stuck at the upper and lower bounds instead of wrapping. This is called [Saturation Arithmetic](https://en.wikipedia.org/wiki/Saturation_arithmetic).

```haskell
  describe "arithmetic" $ do

    it "addition to the maxBound should have no effect" $ do
      let result = maxBound :: Bounds (Inclusive 1) (Exclusive 10)
      result + 1 `shouldBe` result

    it "subtraction from the minBound should have no effect" $ do
      let result = minBound :: Bounds (Inclusive 1) (Exclusive 10)
      result - 1 `shouldBe` result
```

### Serialization

Parsing of closed values is strict.

```haskell
  describe "json" $ do

    it "should successfully parse values in the specified bounds" $ do
      let result = eitherDecode "1" :: Either String (Bounds (Inclusive 1) (Exclusive 10))
      result `shouldBe` Right 1

    it "should fail to parse values outside the specified bounds" $ do
      let result = eitherDecode "0" :: Either String (Bounds (Inclusive 1) (Exclusive 10))
      result `shouldBe` Left "Error in $: parseJSON: Integer 0 is not representable in Closed 1 9"

  describe "csv" $ do

    it "should successfully parse values in the specified bounds" $ do
      let result = CSV.decode CSV.NoHeader "1" :: Either String (Vector (CSV.Only (Bounds (Inclusive 1) (Exclusive 10))))
      result `shouldBe` Right [CSV.Only 1]

    it "should fail to parse values outside the specified bounds" $ do
      let result = CSV.decode CSV.NoHeader "0" :: Either String (Vector (CSV.Only (Bounds (Inclusive 1) (Exclusive 10))))
      result `shouldBe` Left "parse error (Failed reading: conversion error: parseField: Integer 0 is not representable in Closed 1 9) at \"\""

  describe "persistent" $ do

    it "should successfully parse values in the specified bounds" $ do
      let result = fromPersistValue (PersistInt64 1) :: Either Text (Bounds (Inclusive 1) (Exclusive 10))
      result `shouldBe` Right 1

    it "should fail to parse values outside the specified bounds" $ do
      let result = fromPersistValue (PersistInt64 0) :: Either Text (Bounds (Inclusive 1) (Exclusive 10))
      result `shouldBe` Left "fromPersistValue: Integer 0 is not representable in Closed 1 9"
```

### Testing

Closed values can be generated with QuickCheck

```haskell
  describe "quickcheck" $ do

    prop "should always generate values in the specified bounds" $
      \(cx :: Closed 0 1000) ->
        natVal (lowerBound cx) <= getClosed cx &&
        getClosed cx <= natVal (upperBound cx)
```

## Remarks

This library was inspired by [finite-typelits](https://hackage.haskell.org/package/finite-typelits) and [finite-typelits-bounded](https://github.com/pseudonom/finite-typelits-bounded). The differences are summarized below:

* `finite-typelits` - A value of `Finite (n :: Nat)` is in the half-open interval `[0, n)`. Uses modular arithmetic.
* `finite-typelits-bounded` - A value of `Finite (n :: Nat)` is in the half-open interval `[0, n)`. Uses saturation arithmetic.
* `closed` - A value of `Closed (n :: Nat) (m :: Nat)` is in the closed interval `[n, m]`. Uses saturation arithmetic.
