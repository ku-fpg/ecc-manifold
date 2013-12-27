{-# LANGUAGE TupleSections #-}
module ECC.Code.Repetition where

import Data.Bit
import ECC.Types
import Data.Char (isDigit)
import Data.Monoid

-- Simple BSPK encode/decode.
mkHardRepetition :: Int -> ECC
mkHardRepetition n = ECC
        { name     = "repetition/hard/" ++ show n
        , encode   = return . take n . repeat . head
        , decode   = return . (,True) . (: []) . mkBit
                   . (> (n `div` 2)) . length . filter (== 1) . map hard
        , message_length  = 1
        , codeword_length = n
        }
mkSoftRepetition :: Int -> ECC
mkSoftRepetition n = ECC
        { name     = "repetition/soft/" ++ show n
        , encode   = return . take n . repeat . head
        , decode   = return . (,True) . (: []) . mkBit . (> 0) . sum
        , message_length  = 1
        , codeword_length = n
        }


code :: Code
code = Code ["repetition/(hard|soft)/<n>"]
     $ \ xs -> case xs of
                        ["repetition","hard",n] | all isDigit n -> return [mkHardRepetition (read n)]
                        ["repetition","soft",n] | all isDigit n -> return [mkSoftRepetition (read n)]
                        _                                -> return []


