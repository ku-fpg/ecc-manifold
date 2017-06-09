{-# LANGUAGE ScopedTypeVariables, BangPatterns #-}
module ECC.Types
        ( -- * Types
          ECC(..),
          Code(..),
          BEs,          -- abstract
          Log(..),
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
          extractBEs,
          -- * General Utils
          hard,
          soft,
          bitErrorRate
        ) where

import Control.Monad
import Data.List (unfoldr, transpose)
import Data.Monoid
import qualified Data.Vector.Unboxed  as U
import Data.Ratio

-----------------------------------------------------------------------------
-- Regarding Common Synomyns

type MessageLength      = Int          -- the size of the message
type CodewordLength     = Int          -- the size of the message + parity bits
type Rate               = Ratio Int    -- message size / codeword size
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
rateOf :: ECC IO -> Ratio Int
rateOf ecc = message_length ecc % codeword_length ecc

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
--   For example
--     * bit errors: 0 | 1 | 2 | 3
--     * # packets: 97 | 0 | 1 | 2
-- means there were 97 runs that were perfect, one run that had 2 bit errors,
-- and 2 runs that had 3 bit errors. In Haskell, BEs [97,0,1,2]

data BEs = BEs ![Int]
        deriving (Read, Show)

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

-- | Extract all the packet bit errors.
extractBEs :: BEs -> [Double]
extractBEs b@(BEs bes) = expand 0 bes
  where
     expand i [] = []
     expand i (x:xs) = take x (repeat i) ++ expand (i+1) xs


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

--------------------------------------------

-- A serializable log.
data Log 
 = Message (U.Vector Bool)            -- the unencoded packet
 | TxCodeword (U.Vector Bool)         -- the encoded packet
 | RxCodeword EbN0 (U.Vector Double)  -- the encoded packet, after rx/tx (the soft value has lc multiplied in)
 | Decoded String (U.Vector Bool)     -- the packet after decoding (should be the same as the Message)
 deriving (Read,Show)

