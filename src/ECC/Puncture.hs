module ECC.Puncture (punctureECC,punctureTail,punctureTailOfCode) where

import ECC.Types
import Data.Char (isDigit)

-- | 'punctureECC' accepts or rejects bits from a code, shortening the size
-- of the codeword. During decode, the punctured bits are set to unknown (0 :: Double)
-- Because this is refering to elements from a 2D generator matrix, we start at index 1 (not 0).
-- TODO: change the index to 0 (aka orange book)
-- The predicate returns 'True' when we **keep** that specific bit.

punctureECC :: (Int -> Bool) -> ECC -> ECC
punctureECC pred ecc = ecc { name = name ecc ++ "/."
                           , encode = fmap (puncture ps) . encode ecc
                           , decode = decode ecc . unpuncture ps
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

punctureTail :: Int -> ECC -> ECC
punctureTail n ecc = (punctureECC (< (codeword_length ecc - n)) ecc) { name = name ecc ++ "/." ++ show n }

-- | This adds the ability to puncture code, using the '/.128' syntax
punctureTailOfCode :: Code -> Code
punctureTailOfCode (Code nm f) = Code nm $ \ names ->
        case last names of
          '.':ns | all isDigit ns -> fmap (fmap (punctureTail (read ns))) $ f (init names)
          _ -> f names
