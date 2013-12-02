module ECC.Estimate (estimate,showEstimate) where

import ECC.Types
import Data.Bit
import qualified Data.Vector.Unboxed as U
import Statistics.Sample (mean)
import Statistics.Resampling (resample, fromResample)
import Statistics.Resampling.Bootstrap (bootstrapBCA, Estimate(..) )
import System.Random.MWC (create)
import System.Random.MWC
import Numeric

-- Estimate the lower and upper bounds, given a random generator state,
-- a confidence percentage, and a Bit Errors structure.

-- If there are to many samples (as often happens with good error correcting codes),
-- then we cheat, and combine samples.

estimate :: GenIO -> Double -> MessageLength -> BEs -> IO (Maybe Estimate)
estimate g confidence m_len bes
  | sumBEs bes == 0 = return Nothing
  | otherwise =
       do resamples <- resample g [mean] (partitions * 256) sampleU
--          print $ U.length $ fromResample $ head $ resamples
--          print resamples
          return $ Just $ head $ bootstrapBCA confidence sampleU [mean] resamples
  where
    -- We assume there is a power of 2 size, for the shrinking to work
    sample = map (\ be -> be / fromIntegral m_len)
           $ sampleBEs partitions bes
    p xs = length xs > (2 ^ 12) -- todo, stop of not power of 2
    sampleU = U.fromList sample
    partitions = min (sizeBEs bes) 256  -- at the most 256 buckets

-- | Show estimate in an understandable format"
showEstimate :: Estimate -> String
showEstimate est = showEFloat (Just 2) (estPoint est)
                 $ (" +" ++)
                 $ (if estUpperBound est == 0 then ("?" ++)
                    else showsPercent ((estUpperBound est) / (estPoint est) - 1))
                 $ (" -" ++)
                 $ (if estLowerBound est == 0 then ("?" ++)
                    else showsPercent (1 - (estLowerBound est) / (estPoint est)))
                 $ (" [" ++)
                 $ showsPercent (estConfidenceLevel est)
                 $ "]"

showsPercent f = shows (round (100 * f)) . ("%" ++)

-- (1 - (estLowerBound est) / (estPoint est))))





