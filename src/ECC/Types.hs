{-# LANGUAGE ScopedTypeVariables, BangPatterns #-}
module ECC.Types
        ( -- * Types
          ECC(..),
          Code(..),
          BEs,          -- abstract
          -- * Type Synonyms
          MessageLength,
          CodewordLength,
          Rate,
          EbN0,
          -- * 'ECC' function
          rateOf,
          -- * 'BEs' functions
          sumBEs,
          sizeBEs,
          eventBEs,
          sampleBEs,
          -- * General Utils
          hard,
          soft,
          bitErrorRate
        ) where

import Control.Monad
import Data.Bit
import Data.List (unfoldr, transpose)
import Data.Monoid
import qualified Data.Vector.Unboxed  as U


-----------------------------------------------------------------------------
-- Regarding Common Synomyns

type MessageLength      = Int          -- the size of the message
type CodewordLength     = Int          -- the size of the message + parity bits
type Rate               = Rational     -- message size / codeword size
type EbN0               = Double       -- noise

-----------------------------------------------------------------------------
-- Regarding ECC

--
-- | Basic structure of an forward error-checking code.
--
-- Law(s):
--
-- > (encode >>= fmap hard >>= decode) == return
--
--  * length of input to encode, and output of decode == message_length
--
--  * length of output to encode, and input of decode == codeword_length
--

data ECC m = ECC
     { name            :: String
        -- ^ name for the output printer only. The name should have no spaces
     , encode          :: U.Vector Bool       	-> m (U.Vector Bool)
        -- ^ encoded a 'MessageLength' list of bits into a 'CodewordLength' set of bits.
     , decode          :: U.Vector Double       -> m (U.Vector Bool,Bool)
        -- ^ decoding a codeword into a message,
        --  along with a parity flag (True = parity or unknown (assume good), False = bad parity)
     , message_length  :: MessageLength
         -- ^ The length, in bits, of the message (the thing to be sent)
     , codeword_length :: CodewordLength  -- length of w
         -- ^ The length, in bits, of the codeword (the thing that is sent over the air, inc. parity bits)
     }

-- | compute the rate of an 'ECC'.
rateOf :: ECC IO -> Rational
rateOf ecc = fromIntegral (message_length ecc) / fromIntegral (codeword_length ecc)

-----------------------------------------------------------------------------
-- Regarding Code

-- | Code is an encaptuation of an 'ECC' builder.
-- It has a list/description of possible codes,
-- and a mapping from expanded code-name (/ is the seperator),
-- to possible 'EEC's.
data Code = Code [String] ([String] -> IO [ECC IO])

instance Show Code where
        show (Code codes _) = show codes

instance Monoid Code where
  mempty = Code [] $ \ _ -> return []
  mappend (Code c1 f1) (Code c2 f2) = Code (c1 ++ c2) $ \ xs -> liftM2 (++) (f1 xs) (f2 xs)

-----------------------------------------------------------------------------
-- Regarding Bit Errors (BEs)


-- | 'BEs' is a summary of bit errors in multiple runs.
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


-- | The total numbers of bit errors
sumBEs :: BEs -> Int
sumBEs (BEs xs) = sum [ n * i | (n,i) <- xs `zip` [0..]]

-- | The number of samples that have been run.
sizeBEs :: BEs -> Int
sizeBEs (BEs xs) = sum xs

-- | Build n buckets for the BEs. Each one contains
-- a bit error rate for message length of 1.
-- Assumes that sumBEs bes % n == 0
--
-- Example:
--
--  * sampleBEs 1 bes gets the current bit error rate (for message length == 1).
--
--  * sampleBEs 256 bes gets 256 buckets, each one with a representative bit error rate.
--
-- Property:
--
--  * average (sampleBEs n bes) == average (sampleBEs m bes)  (for reasonable n,m)
--
sampleBEs :: Int -> BEs -> [Double]
sampleBEs n (BEs (zeros:bes)) = map sum' $ transpose $ unfoldr f xs
   where
        bes' = (zeros `mod` n) : bes
        f ys = case (take n ys, drop n ys) of
                 (vs,ws) | length vs == 0 -> Nothing
                         | length vs < n  -> error "bad bucket size for sampleBEs"
                         | otherwise      -> Just (vs,ws)
        xs = concat $ zipWith (\ n i -> take n (repeat i)) bes' [0..]
        sum' xs = fromIntegral (sum xs) / (fromIntegral (length xs + (zeros `div` n)))


-- | turn an event with a specific number of bit errors (possibly 0),
-- and give a singleton 'BEs'.
eventBEs :: Int -> BEs
eventBEs n = BEs $ take n (repeat 0) ++ [1]

{-
-- for +ve ints
prop_ECCBERS (xs :: [Int]) = sum xs == sumBEs (mconcat (map eventBEs xs))
-}

-----------------------------------------------------------------------------
-- Regarding Bit.

-- | turn a soft value into a hard value.
hard :: (Num a, Ord a) => a -> Bool
hard = (> 0)

-- | turn a hard value into a soft value.
soft :: (Num a) => Bool -> a
soft False = -1
soft True  = 1

-- | compute the bit error rate inside a 'BEs', for a specific 'ECC'.
bitErrorRate :: ECC f -> BEs -> Double
bitErrorRate ecc bes = fromIntegral (sumBEs bes) / (fromIntegral (sizeBEs bes * message_length ecc))
