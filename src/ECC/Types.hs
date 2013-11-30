{-# LANGUAGE ScopedTypeVariables, BangPatterns #-}
module ECC.Types where

import Control.Monad
import Data.Bit
import Data.Monoid

-----------------------------------------------------------------------------
-- Regarding Common Synomyns

type MessageLength      = Int          -- the size of the message
type CodewordLength     = Int          -- the size of the message + parity bits
type Rate               = Rational     -- message size / codeword size
type EbN0               = Double       -- noise

-----------------------------------------------------------------------------
-- Regarding ECC

-- Basic structure of an forward error-checking code.
--
-- Laws:
--
-- > (encode >>= fmap hard >>= decode) == return
-- >
--
-- * length of input to encode, and output of decode == message_length
-- * length of output to encode, and input of decode == codeword_length

data ECC = ECC
     { name            :: String
     , encode          :: [Bit]       	-> IO [Bit]
        -- ^ encoded a 'MessageLength' list of bits into a 'CodewordLength' set of bits.
     , decode          :: [Double] 	-> IO ([Bit],Bool)
        -- ^ decoding a codeword into a message,
        --  along with a parity flag (True = parity or unknown (assume good), False = bad parity)
     , message_length  :: MessageLength   -- length of v
     , codeword_length :: CodewordLength  -- length of w
     }

rateOf :: ECC -> Rational
rateOf ecc = fromIntegral (message_length ecc) / fromIntegral (codeword_length ecc)

-----------------------------------------------------------------------------
-- Regarding Code

data Code = Code [String] ([String] -> IO [ECC])

instance Show Code where
        show (Code codes _) = show codes

instance Monoid Code where
  mempty = Code [] $ \ _ -> return []
  mappend (Code c1 f1) (Code c2 f2) = Code (c1 ++ c2) $ \ xs -> liftM2 (++) (f1 xs) (f2 xs)

-----------------------------------------------------------------------------
-- Regarding Bit Errors (BEs)


data BEs = BEs ![Int]
        deriving Show

-- A space efficent version of
instance Monoid BEs where
        mempty = BEs []
        mappend (BEs xs) (BEs ys) = BEs $! f xs ys
          where f (x:xs) (y:ys) =  (x + y) `cons` f xs ys
                f xs     []     = xs
                f []     ys     = ys

                cons !x !ys = x : ys


-- The total numbers of bit errors
sumBEs :: BEs -> Int
sumBEs (BEs xs) = sum [ n * i | (n,i) <- xs `zip` [0..]]

-- The number of samples
sizeBEs :: BEs -> Int
sizeBEs (BEs xs) = sum xs

-- Build n buckets for the BEs.
-- Assumes that sumBEs bes % n == 0
sampleBEs :: Int -> BEs -> [Double]
sampleBEs n bes = []

eventBEs :: Int -> BEs
eventBEs n = BEs $ take n (repeat 0) ++ [1]

-- for +ve ints
prop_ECCBERS (xs :: [Int]) = sum xs == sumBEs (mconcat (map eventBEs xs))

-----------------------------------------------------------------------------
-- Regarding Bit.

hard :: (Num a, Ord a) => a -> Bit
hard = mkBit . (> 0)

soft :: (Num a) => Bit -> a
soft 0 = -1
soft 1 = 1

bitErrorRate :: ECC -> BEs -> Double
bitErrorRate ecc bes = fromIntegral (sumBEs bes) / (fromIntegral (sizeBEs bes * message_length ecc))
