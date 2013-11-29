{-# LANGUAGE ScopedTypeVariables, BangPatterns #-}
module ECC.Types where

import Data.Bit
import Data.Monoid

type MessageLength      = Int          -- the size of the message
type CodewordLength     = Int          -- the size of the message + parity bits
type Rate               = Rational     -- message size / codeword size
type EbN0               = Double       -- noise

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


data Code = Code ([String] -> [ECC])

instance Monoid Code where
  mempty = Code $ \ _ -> []
  mappend (Code f1) (Code f2) = Code $ \ xs -> f1 xs ++ f2 xs

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
