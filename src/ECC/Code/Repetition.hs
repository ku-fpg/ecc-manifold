{-# LANGUAGE TupleSections #-}
module ECC.Code.Repetition where

import Data.Bit
import ECC.Types
import Data.Char (isDigit)

-- Simple BSPK encode/decode.
mkRepetition :: Int -> ECC
mkRepetition n = ECC
        { name     = "repetition/" ++ show n
        , encode   = return . take n . repeat . head
        , decode   = return . (,True) . (: []) . mkBit
                   . (> (n `div` 2)) . length . filter (== 1) . map hard
--                     . (> 0) . sum
        , message_length  = 1
        , codeword_length = n
        }

code :: Code
code = Code ["repetition/<n>"]
     $ \ xs -> case xs of
                        ["repetition",n] | all isDigit n -> return [mkRepetition (read n)]
                        _                                -> return []


