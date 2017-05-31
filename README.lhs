# closed: Integers bounded by a closed interval

## Build

  ```plaintext
  stack build
  ```

## Tutorial

### Overview

This package exports one core data type `Closed (n :: Nat) (m :: Nat)`
for describing integers bounded by a closed interval. That is, given
`cx :: Closed n m`, `getClosed cx` is an integer `x` where `n <= x <= m`.

We also export a type family `Range` for describing open and half-open
intervals in terms of closed intervals.

  ```plaintext
  Range (Includes 0) (Includes 10) => Closed 0 10
  Range (Includes 0) (Excludes 10) => Closed 0 9
  Range (Excludes 0) (Includes 10) => Closed 1 10
  Range (Excludes 0) (Excludes 10) => Closed 1 9
  ```

### Preamble

  ```haskell
  {-# LANGUAGE TypeFamilies #-}
  {-# LANGUAGE DataKinds #-}
  {-# LANGUAGE OverloadedStrings #-}
  {-# LANGUAGE OverloadedLists #-}
  {-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

  module Main where

  import Closed
  import Control.Exception
  import Data.Aeson
  import qualified Data.Csv as CSV
  import Data.Vector
  import Data.Proxy
  import Test.Hspec

  main :: IO ()
  main = hspec $ do
  ```

### Construction

  The safe constructor `packClosed` uses `Maybe` to indicate failure. There is
  also an unsafe constructor `closed` as well as a `Num` instance that implements
  `fromInteger`.

  ```haskell
    describe "safe construction" $ do

      it "should successfully construct values in the specified range" $ do
        let result = packClosed 2 :: Maybe (Range (Includes 2) (Excludes 5))
        getClosed <$> result `shouldBe` Just 2

      it "should fail to construct values outside the specified range" $ do
        let result = packClosed 1 :: Maybe (Range (Includes 2) (Excludes 5))
        getClosed <$> result `shouldBe` Nothing

    describe "unsafe construction" $ do

      it "should successfully construct values in the specified range" $ do
        let result = closed 2 :: Range (Includes 2) (Excludes 5)
        getClosed result `shouldBe` 2

      it "should fail to construct values outside the specified range" $ do
        let result = closed 1 :: Range (Includes 2) (Excludes 5)
        evaluate (getClosed result) `shouldThrow` anyErrorCall

    describe "unsafe literal construction" $ do

      it "should successfully construct values in the specified range" $ do
        let result = 2 :: Range (Includes 2) (Excludes 5)
        getClosed result `shouldBe` 2

      it "should fail to construct values outside the specified range" $ do
        let result = 1 :: Range (Includes 2) (Excludes 5)
        evaluate (getClosed result) `shouldThrow` anyErrorCall
  ```

### Elimination

  Use `getClosed` to extract the `Integer` from a `Closed` value.

  ```haskell
    describe "elimination" $ do

      it "should allow the integer value to be extracted" $ do
        let result = 1 :: Range (Includes 0) (Excludes 10)
        getClosed result `shouldBe` 1
  ```

### Bounds Manipulation

  The upper and lower bounds can be queried, strengthened, and weakened.

  ```haskell
    describe "bounds manipulation" $ do

      let cx = 2 :: Range (Includes 1) (Excludes 10)

      it "should allow querying the bounds" $ do
        upperBound cx `shouldBe` (Proxy :: Proxy 9)
        lowerBound cx `shouldBe` (Proxy :: Proxy 1)

      it "should allow weakening the bounds" $ do
        upperBound (weakenUpper cx) `shouldBe` (Proxy :: Proxy 10)
        lowerBound (weakenLower cx) `shouldBe` (Proxy :: Proxy 0)

      it "should allow strengthening the bounds" $ do
        upperBound <$> strengthenUpper cx `shouldBe` Just (Proxy :: Proxy 8)
        lowerBound <$> strengthenLower cx `shouldBe` Just (Proxy :: Proxy 2)
  ```

### Arithmetic

  Arithmetic gets stuck at the upper and lower bounds instead of wrapping.

  ```haskell
    describe "arithmetic" $ do

      it "addition to the maxBound should have no effect" $ do
        let result = maxBound :: Range (Includes 1) (Excludes 10)
        result + 1 `shouldBe` result


      it "subtraction from the minBound should have no effect" $ do
        let result = minBound :: Range (Includes 1) (Excludes 10)
        result - 1 `shouldBe` result
  ```

### Serialization

  Parsing of closed values is strict.

  ```haskell
    describe "json" $ do

      it "should successfully parse values in the specified range" $ do
        let result = eitherDecode "1" :: Either String (Range (Includes 1) (Excludes 10))
        result `shouldBe` Right 1

      it "should fail to parse values outside the specified range" $ do
        let result = eitherDecode "0" :: Either String (Range (Includes 1) (Excludes 10))
        result `shouldBe` Left "Error in $: parseJSON: Integer 0 is not representable in Closed 1 9"

    describe "csv" $ do

      it "should successfully parse values in the specified range" $ do
        let result = CSV.decode CSV.NoHeader "1" :: Either String (Vector (CSV.Only (Range (Includes 1) (Excludes 10))))
        result `shouldBe` Right [CSV.Only 1]

      it "should fail to parse values outside the specified range" $ do
        let result = CSV.decode CSV.NoHeader "0" :: Either String (Vector (CSV.Only (Range (Includes 1) (Excludes 10))))
        result `shouldBe` Left "parse error (Failed reading: conversion error: parseField: Integer 0 is not representable in Closed 1 9) at \"\""
  ```

## Remarks

This library was inspired by [finite-typelits](https://hackage.haskell.org/package/finite-typelits)
and [finite-typelits-bounded](https://github.com/pseudonom/finite-typelits-bounded). The differences
are summarized below:

* `finite-typelits` - A value of `Finite (n :: Nat)` is in the half-open interval `[0, n)`. Uses modular arithmetic.
* `finite-typelits-bounded` - A value of `Finite (n :: Nat)` is in the half-open interval `[0, n)`. Uses bounded arithmetic.
* `closed` - A value of `Closed (n :: Nat) (m :: Nat)` is in the closed interval `[n, m]`. Uses bounded arithmetic.
