module ECC.Types where

import Data.Bit
import Data.Monoid

type MessageLength      = Int          -- the size of the message
type CodewordLength     = Int          -- the size of the message + parity bits
type Rate               = Rational     -- message size / codeword size

-- Basic structure of an forward error-checking code.
--
-- Laws:
--
-- > (encode >>= fmap hard >>= decode) == return
-- >
--
-- * length of input to encode, and output of decode == message_length
-- * length of output to encode, and input of decode == codeword_length

data ECC = ECC
     { encode          :: [Bit]       	-> IO [Bit]
        -- ^ encoded a 'MessageLength' list of bits into a 'CodewordLength' set of bits.
     , decode          :: [Double] 	-> IO [Bit]
     , message_length  :: MessageLength   -- length of v
     , codeword_length :: CodewordLength  -- length of w
     }


data Code = Code ([String] -> [ECC])

instance Monoid Code where
  mempty = Code $ \ _ -> []
  mappend (Code f1) (Code f2) = Code $ \ xs -> f1 xs ++ f2 xs

