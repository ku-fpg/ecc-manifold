{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      : Data.Bit
-- Copyright   : (c) The University of Kansas
-- License     : BSD-style
--
-- Maintainer  : andygill@ku.edu
-- Stability   : experimental
-- Portability : portable
--
-- This implements a single bit numerical value, aka a 'Word1'.
--

module Data.Bit (
        -- * Data Types
        Bit, Word1,
        -- * Construction and extraction
        mkBit, getBit,
        -- * Utils
        sumBits
  ) where

import Data.Bits

-- A 'Bit' is 'Zero' or 'One'
data Bit = Zero | One
        deriving (Eq, Ord, Enum)

type Word1 = Bit

-- | create a 'Bit'
mkBit :: Bool -> Bit
mkBit False = Zero
mkBit True  = One

-- | extract a 'Bit'
getBit :: Bit -> Bool
getBit Zero = False
getBit One  = True

-- | summate the 1-'Bit's
sumBits :: [Bit] -> Int
sumBits bs = length [ () | One <- bs ]

instance Show Bit where
    show Zero = "0"
    show One  = "1"

instance Read Bit where
    readsPrec p xs = case readsPrec p xs of
                       [ (0::Int,ys) ] -> [ (Zero,ys) ]
                       [ (1::Int,ys) ] -> [ (One,ys) ]
                       _ -> []

instance Num Bit where
    a + b = mkBit (a /= b)        -- XOR
    One * One  = One              -- AND
    _   * _    = Zero
    a   - Zero = a
    Zero - One = One
    One  - One = Zero
    negate a   = a
    abs    a   = a
    signum a   = a

    fromInteger 0 = Zero
    fromInteger 1 = One
    fromInteger n = error $ show n ++ " :: Bit is not 0 or 1"

instance Real Bit where
    toRational Zero = 0
    toRational One  = 1

instance Integral Bit where
  toInteger Zero = 0
  toInteger One = 1
  quotRem x One = (x, Zero)
  quotRem _ Zero = error "divide by zero"

instance Bits Bit where
    (.&.) = (*)
    Zero .|. Zero = Zero
    _   .|.  _    = One
    xor = (+)
    complement Zero = One
    complement One = Zero
    shift a 0 = a
    shift _ _ = 0
    rotate a _ = a
    bit 0 = 1
    bit _ = 0
    bitSize _ = 1
    testBit b 0 = getBit b
    testBit _ _ = error "testBit out of range (0 .. 0)"
    isSigned _ = True
    popCount Zero = 0
    popCount One = 1

