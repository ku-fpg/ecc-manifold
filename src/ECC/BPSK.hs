{-# LANGUAGE TupleSections #-}
module ECC.BPSK where

import Data.Bit
import ECC.Types
import ECC.Utils
import Data.Char (isDigit)

-- Simple BSPK encode/decode.

mkBPSK :: Int -> ECC
mkBPSK n = ECC
        { name     = "bspk/" ++ show n
        , encode   = return
        , decode   = return . (,True) . fmap mkBit . fmap (>= 0)
        , message_length  = n
        , codeword_length = n
        }

code :: Code
code = Code $ \ xs -> case xs of
                        ["bpsk",n] | all isDigit n -> [mkBPSK (read n)]
                        _                          -> []
