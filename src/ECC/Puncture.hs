module ECC.Puncture (punctureECC,punctureTail,punctureTailOfCode) where

import ECC.Types
import Data.Char (isDigit)
import qualified Data.Vector.Unboxed  as U

-- | 'punctureECC' accepts or rejects bits from a code, shortening the size
-- of the codeword. During decode, the punctured bits are set to unknown (0 :: Double)
-- Because this is refering to elements from a 2D generator matrix, we start at index 1 (not 0).
-- TODO: change the index to 0 (aka orange book)
-- The predicate returns 'True' when we **keep** that specific bit.

punctureECC :: Functor f => (Int -> Bool) -> ECC f -> ECC f
punctureECC pred ecc = ecc { name = name ecc ++ "/."
                           , encode = fmap (U.fromList . puncture ps . U.toList) . encode ecc
                           , decode = decode ecc . U.fromList . unpuncture ps . U.toList
                           , codeword_length = new_codeword_length
                           }
  where
        ps = map pred [0..codeword_length ecc-1]
        new_codeword_length = length $ filter id ps

puncture :: [Bool] -> [a] -> [a]
puncture ns xs = [ b | (b,True) <- xs `zip` ns]

unpuncture :: [Bool] -> [Double] -> [Double]
unpuncture []         _      = []
unpuncture (False:ns) xs     = 0 : unpuncture ns xs
unpuncture (n    :ns) (x:xs) = x : unpuncture ns xs

punctureTail :: Functor f => Int -> ECC f -> ECC f
punctureTail n ecc = (punctureECC (< (codeword_length ecc - n)) ecc) { name = name ecc ++ "/." ++ show n }

-- | This adds the ability to puncture code, using the '/.128' syntax
punctureTailOfCode :: Code -> Code
punctureTailOfCode (Code nm initialize finalize f) = Code nm initialize finalize $ \ vars names ->
        case last names of
          '.':ns | all isDigit ns -> fmap (fmap (punctureTail (read ns))) $ f vars (init names)
          _ -> f vars names
