module ECC.Utils (rateOf, hard, soft, bitErrorRate, nintyFifth) where

import ECC.Types
import Data.Bit
import qualified Data.Vector.Unboxed as U
import Statistics.Sample (mean)
import Statistics.Resampling (resample, fromResample)
import Statistics.Resampling.Bootstrap (bootstrapBCA, Estimate(..) )
import System.Random.MWC (create)
import System.Random.MWC

rateOf :: ECC -> Rational
rateOf ecc = fromIntegral (message_length ecc) / fromIntegral (codeword_length ecc)

hard :: (Num a, Ord a) => a -> Bit
hard = mkBit . (> 0)

soft :: (Num a) => Bit -> a
soft 0 = -1
soft 1 = 1

bitErrorRate :: ECC -> BEs -> Double
bitErrorRate ecc bes = fromIntegral (sumBEs bes) / (fromIntegral (sizeBEs bes * message_length ecc))

nintyFifth :: GenIO -> MessageLength -> BEs -> IO Estimate
nintyFifth g m_len (BEs xs) = do
          resamples <- resample g [mean] 10000 sampleU -- (length sample^2) sampleU
--          print $ U.length $ fromResample $ head $ resamples
--          print resamples
          return $ head $ bootstrapBCA 0.95 sampleU [mean] resamples
  where
    -- We assume there is a power of 2 size, for the shrinking to work
    sample = head
           $ dropWhile p
           $ iterate combine
           $ map (\ be -> fromIntegral be / fromIntegral m_len)
           $ concat
           $ zipWith (\ n i -> take n (repeat i) ) xs [0..]
    p xs = length xs > (2 ^ 12) -- todo, stop of not power of 2
    sampleU = U.fromList sample


--combine :: (Num a) => [a] -> [a]
combine (x:y:xs) = ((x + y) / 2) : combine xs
combine xs = xs

