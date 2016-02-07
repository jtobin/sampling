{-# OPTIONS_GHC -Wall #-}

module Numeric.Sampling (
    -- * Without replacement
    sample
  , sampleIO
  ) where

import qualified Control.Foldl             as F
import           Control.Monad.Primitive   (PrimMonad, PrimState)
import qualified Data.Vector               as V
import           Numeric.Sampling.Internal (randomN)
import           System.Random.MWC

-- | (/O(n)/) Sample uniformly, without replacement.
--
--   Returns Nothing if the desired sample size is larger than the collection
--   being sampled from.
sample
  :: (PrimMonad m, Foldable f)
  => Int -> f a -> Gen (PrimState m) -> m (Maybe (V.Vector a))
sample n xs gen
  | n < 0     = return Nothing
  | otherwise = F.foldM (randomN n gen) xs

-- | (/O(n)/) Sample uniformly without replacement, specialized to IO.
sampleIO :: Foldable f => Int -> f a -> IO (Maybe (V.Vector a))
sampleIO n xs = do
  gen <- createSystemRandom
  sample n xs gen


probSample = undefined
probSampleIO = undefined
resample = undefined
resampleIO = undefined
probResample = undefined
probResampleIO = undefined
-- streams?


