{-# LANGUAGE TupleSections #-}
module ECC.Code.Repetition where

import Data.Bit
import ECC.Types
import Data.Char (isDigit)
import Data.Monoid

-- Simple BSPK encode/decode.
mkHardRepetition :: Applicative f => Int -> ECC f
mkHardRepetition n = ECC
        { name     = "repetition/hard/" ++ show n
        , encode   = pure . take n . repeat . head
        , decode   = pure . (,True) . (: []) . fromBool
                   . (> (n `div` 2)) . length . filter (== 1) . map hard
        , message_length  = 1
        , codeword_length = n
        }
mkSoftRepetition :: Applicative f => Int -> ECC f
mkSoftRepetition n = ECC
        { name     = "repetition/soft/" ++ show n
        , encode   = pure . take n . repeat . head
        , decode   = pure . (,True) . (: []) . fromBool . (> 0) . sum
        , message_length  = 1
        , codeword_length = n
        }


code :: Code
code = Code ["repetition/(hard|soft)/<n>"]
     $ \ xs -> case xs of
                        ["repetition","hard",n] | all isDigit n -> return [mkHardRepetition (read n)]
                        ["repetition","soft",n] | all isDigit n -> return [mkSoftRepetition (read n)]
                        _                                -> return []


