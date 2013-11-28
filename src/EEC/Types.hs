module FEC.Types where

import Data.Bit

type MessageLength      = Int          -- the size of the message
type CodewordLength     = Int          -- the size of the message + parity bits
type Rate               = Rational     -- message size / codeword size

-- Basic structure of an forward error-checking code.
--
-- > Law: (encode >>= fmap soft >>= decode) == return
--

data FEC m = FEC
     { encode          :: [Bit]       	-> m [Bit]
        -- ^ encoded a 'MessageLength' list of bits into a 'CodewordLength' set of bits.
     , decode          :: [Double] 	-> m [Bit]
     , message_length  :: MessageLength   -- length of v
     , codeword_length :: CodewordLength  -- length of w
     }

