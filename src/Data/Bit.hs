module Data.Bit where

import Control.Monad
import Data.Bits

data Bit = Zero | One
        deriving (Eq, Ord, Enum)

type Word1 = Bit

mkBit :: Bool -> Bit
mkBit False = Zero
mkBit True  = One

getBit :: Bit -> Bool
getBit Zero = False
getBit One  = True

sumBits :: [Bit] -> Int
sumBits bs = length [ () | One <- bs ]

instance Show Bit where
    show Zero = "0"
    show One  = "1"

instance Read Bit where
    readsPrec p xs = case readsPrec p xs of
                       [ (0,ys) ] -> [ (Zero,ys) ]
                       [ (1,ys) ] -> [ (One,ys) ]
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
    isSigned _ = True
    popCount Zero = 0
    popCount One = 1

