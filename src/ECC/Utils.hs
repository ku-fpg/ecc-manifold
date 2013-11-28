module ECC.Utils where

import ECC.Types
import Data.Bit

rateOf :: ECC -> Rational
rateOf ecc = fromIntegral (message_length ecc) / fromIntegral (codeword_length ecc)

hard :: (Num a, Ord a) => a -> Bit
hard = mkBit . (> 0)

soft :: (Num a) => Bit -> a
soft 0 = -1
soft 1 = 1

