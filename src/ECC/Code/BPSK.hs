{-# LANGUAGE TupleSections #-}
module ECC.Code.BPSK where

import Data.Bit
import ECC.Types
import Data.Char (isDigit)

-- Simple BSPK encode/decode.

mkBPSK :: ECC
mkBPSK = ECC
        { name     = "bspk"
        , encode   = return
        , decode   = return . (,True) . fmap mkBit . fmap (>= 0)
        , message_length  = 1
        , codeword_length = 1
        }

code :: Code
code = Code $ \ xs -> case xs of
                        ["bpsk"]                   -> [mkBPSK]        -- the default
                        _                          -> []
