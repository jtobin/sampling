{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE FlexibleContexts #-}

module Numeric.Sampling (
    -- * Without replacement
    sample
  , sampleIO

    -- * With replacement
  , resample
  , resampleIO

    -- * Unequal probability, with replacement
  , presample
  , presampleIO
  ) where

import qualified Control.Foldl               as F
import           Control.Monad.Primitive     (PrimMonad, PrimState)
import qualified Data.Foldable               as Foldable
import           Data.Maybe                  (fromJust)
import           Data.Vector                 (Vector)
import qualified Data.Vector                 as V
import           Numeric.Sampling.Internal
import           System.Random.MWC

-- | (/O(n)/) Sample uniformly, without replacement.
--
--   Returns Nothing if the desired sample size is larger than the collection
--   being sampled from.
sample
  :: (PrimMonad m, Foldable f)
  => Int -> f a -> Gen (PrimState m) -> m (Maybe (Vector a))
sample n xs gen
  | n < 0     = return Nothing
  | otherwise = F.foldM (randomN n gen) xs
{-# INLINABLE sample #-}

-- | (/O(n)/) 'sample' specialized to IO.
sampleIO :: Foldable f => Int -> f a -> IO (Maybe (Vector a))
sampleIO n xs = do
  gen <- createSystemRandom
  sample n xs gen
{-# INLINABLE sampleIO #-}

-- | (/O(n log n)/) Sample uniformly with replacement (bootstrap).
resample
  :: (PrimMonad m, Foldable f)
  => Int -> f a -> Gen (PrimState m) -> m (Vector a)
resample n xs = presample n weighted where
  weight   = recip (F.fold F.genericLength xs)
  weighted = zip (repeat weight) (Foldable.toList xs)
{-# INLINABLE resample #-}

-- | (/O(n log n)/) 'resample' specialized to IO.
resampleIO :: (Foldable f) => Int -> f a -> IO (Vector a)
resampleIO n xs = do
  gen <- createSystemRandom
  resample n xs gen
{-# INLINABLE resampleIO #-}

-- | (/O(n log n)/) Unequal probability resampling.
presample
  :: (PrimMonad m, Foldable f)
  => Int -> f (Double, a) -> Gen (PrimState m) -> m (Vector a)
presample n weighted gen
    | n <= 0    = return V.empty
    | otherwise = do
        let vweighted = V.fromList $ Foldable.toList weighted
        sorted <- mutableSortByProbability vweighted
        let probs     = drop 1 (F.scan (F.premap fst F.sum) (V.toList sorted))
            cdf       = V.zip (V.fromList probs) (V.map snd sorted)
        accumulateSample n cdf gen
  where
    accumulateSample
      :: (PrimMonad m)
      => Int -> Vector (Double, a) -> Gen (PrimState m) -> m (Vector a)
    accumulateSample size xs g = go V.empty size where
      go !acc s
        | s <= 0    = return $! acc
        | otherwise = do
            z <- uniform g
            let pair   = F.fold (F.find ((>= z) . fst)) xs
                result = snd . fromJust $ pair -- FIXME
            go (V.cons result acc) (pred s)
{-# INLINABLE presample #-}

-- | (/O(n log n)/) 'presample' specialized to IO.
presampleIO :: (Foldable f) => Int -> f (Double, a) -> IO (Vector a)
presampleIO n weighted = do
  gen <- createSystemRandom
  presample n weighted gen
{-# INLINABLE presampleIO #-}

