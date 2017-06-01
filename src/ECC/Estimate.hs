module ECC.Estimate (estimate,showEstimate) where

import ECC.Types
import qualified Data.Vector.Unboxed as U
import Statistics.Sample (mean)
import Statistics.Resampling (resample, fromResample, Estimator(..))
import Statistics.Resampling.Bootstrap (bootstrapBCA)
import Statistics.Types (Estimate (..), ConfInt, mkCL, ConfInt(..), confidenceLevel)
import System.Random.MWC (create)
import System.Random.MWC
import Numeric

-- Estimate the lower and upper bounds, given a random generator state,
-- a confidence percentage, and a Bit Errors structure.

-- If there are to many samples (as often happens with good error correcting codes),
-- then we cheat, and combine samples.

estimate :: GenIO -> Double -> MessageLength -> BEs -> IO (Maybe (Estimate ConfInt Double))
estimate g confidence m_len bes
  | sumBEs bes == 0 = return Nothing
  | otherwise =
       do resamples <- resample g [Mean] (partitions * 256) sampleU
--          print $ U.length $ fromResample $ head $ resamples
--          print resamples
          return $ Just $ head $ bootstrapBCA (mkCL confidence) sampleU {- [Mean] -} resamples
  where
    -- We assume there is a power of 2 size, for the shrinking to work
    sample = map (\ be -> be / fromIntegral m_len)
           $ sampleBEs partitions bes
    p xs = length xs > (2 ^ 12) -- todo, stop of not power of 2
    sampleU = U.fromList sample
    partitions = min (sizeBEs bes) 256  -- at the most 256 buckets

-- | Show estimate in an understandable format"
showEstimate :: Estimate ConfInt Double -> String
showEstimate est = showEFloat (Just 2) (estPoint est)
                 $ (" +" ++)
                 $ (if confIntLDX estErr == 0 then ("?" ++)
                    else showsPercent ((confIntUDX estErr) / (estPoint est)))
                 $ (" -" ++)
                 $ (if confIntUDX estErr == 0 then ("?" ++)
                    else showsPercent ((confIntLDX estErr) / (estPoint est)))
                 $ (" " ++)
                 $ showsPercent (confidenceLevel (confIntCL estErr))
                 $ ""
  where
    estErr = estError est

showsPercent f = shows (round (100 * f)) . ("%" ++)

-- (1 - (estLowerBound est) / (estPoint est))))





