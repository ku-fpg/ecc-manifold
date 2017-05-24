{-# LANGUAGE TupleSections #-}
module ECC.Code.BPSK where

import Data.Bit
import ECC.Types
import Data.Char (isDigit)

-- Simple BSPK encode/decode.

mkBPSK :: Applicative f => ECC f
mkBPSK = ECC
        { name     = "bspk"
        , encode   = pure
        , decode   = pure . (,True) . fmap hard
        , message_length  = 1
        , codeword_length = 1
        }

code :: Code
code = Code ["bpsk"]
     $ \ xs -> case xs of
                        ["bpsk"]                   -> return [mkBPSK]        -- the default
                        _                          -> return []
