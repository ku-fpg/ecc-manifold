module ECC.Estimate (estimate) where

import ECC.Types
import Data.Bit
import qualified Data.Vector.Unboxed as U
import Statistics.Sample (mean)
import Statistics.Resampling (resample, fromResample)
import Statistics.Resampling.Bootstrap (bootstrapBCA, Estimate(..) )
import System.Random.MWC (create)
import System.Random.MWC

-- Estimate the lower and upper bounds, given a random generator state,
-- a confidence percentage, and a Bit Errors structure.

-- If there are to many samples (as often happens with good error correcting codes),
-- then we cheat, and combine samples.

estimate :: GenIO -> Double -> MessageLength -> BEs -> IO Estimate
estimate g confidence m_len (BEs xs) = do
          resamples <- resample g [mean] 10000 sampleU -- (length sample^2) sampleU
--          print $ U.length $ fromResample $ head $ resamples
--          print resamples
          return $ head $ bootstrapBCA confidence sampleU [mean] resamples
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

