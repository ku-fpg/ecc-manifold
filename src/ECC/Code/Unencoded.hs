{-# LANGUAGE TupleSections #-}
module ECC.Code.Unencoded where

import ECC.Types
import Data.Char (isDigit)
import qualified Data.Vector.Unboxed  as U

-- Unencoded encode/decode.

mkUnencoded :: Applicative f => Int -> ECC f
mkUnencoded n = ECC
        { name            = "unencoded/" ++ show n
        , encode          = pure
        , decode          = pure . (,True) . U.map hard
        , message_length  = n
        , codeword_length = n
        }

code :: Code
code = Code ["unencoded/<message-length>"] (pure ()) (const (pure ()))
     $ \ vars xs -> case xs of
                        ["unencoded",n]  | all isDigit n
                                         -> return [mkUnencoded (read n)]  
                        _                -> return []
