{-# LANGUAGE TupleSections #-}
module ECC.Code.Repetition where

import ECC.Types
import Data.Char (isDigit)
import Data.Monoid
import qualified Data.Vector.Unboxed  as U


-- Simple BSPK encode/decode.
mkHardRepetition :: Applicative f => Int -> ECC f
mkHardRepetition n = ECC
        { name     = "repetition/hard/" ++ show n
        , encode   = pure . U.fromList . take n . repeat . head . U.toList
        , decode   = pure . (,True) . U.fromList . (: []) 
                   . (> (n `div` 2)) . length . filter id . map hard . U.toList
        , message_length  = 1
        , codeword_length = n
        }
mkSoftRepetition :: Applicative f => Int -> ECC f
mkSoftRepetition n = ECC
        { name     = "repetition/soft/" ++ show n
        , encode   = pure . U.fromList . take n . repeat . head . U.toList
        , decode   = pure . (,True) . U.fromList . (: []) . hard . sum . U.toList
        , message_length  = 1
        , codeword_length = n
        }


code :: Code
code = Code ["repetition/(hard|soft)/<n>"]
     $ \ xs -> case xs of
                        ["repetition","hard",n] | all isDigit n -> return [mkHardRepetition (read n)]
                        ["repetition","soft",n] | all isDigit n -> return [mkSoftRepetition (read n)]
                        _                                -> return []


