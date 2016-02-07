{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}

module Numeric.Sampling (
    -- * Without replacement
    sample
  , sampleIO

    -- * With replacement
  , resample
  , resampleIO

    -- * Unequal probability sampling without replacement
  , probSample
  , probSampleIO

    -- * Unequal probability sampling with replacement
  , probResample
  , probResampleIO
  ) where

import qualified Control.Foldl             as F
import           Control.Monad.Primitive   (PrimMonad, PrimState)
import qualified Data.Foldable             as Foldable
import           Data.Function             (on)
import           Data.List.Ordered         (mergeBy)
import           Data.Maybe                (fromJust) -- FIXME
import           Data.Vector               (Vector)
import qualified Data.Vector               as V
import qualified Data.Vector.Algorithms.Merge as V
import           Numeric.Sampling.Internal (randomN)
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

-- | (/O(n)/) 'sample' specialized to IO.
sampleIO :: Foldable f => Int -> f a -> IO (Maybe (Vector a))
sampleIO n xs = do
  gen <- createSystemRandom
  sample n xs gen

-- | (/O(n)/) Sample uniformly with replacement (bootstrap).
resample
  :: (PrimMonad m, Foldable f)
  => Int -> f a -> Gen (PrimState m) -> m (Vector a)
resample n xs = probResample n weighted where
  weight   = recip (F.fold F.genericLength xs)
  weighted = zip (repeat weight) (Foldable.toList xs)

-- | (/O(n)/) 'resample' specialized to IO.
resampleIO :: Foldable f => Int -> f a -> IO (Vector a)
resampleIO n xs = do
  gen <- createSystemRandom
  resample n xs gen





probResample
  :: (PrimMonad m, Foldable f)
  => Int -> f (Double, a) -> Gen (PrimState m) -> m (Vector a)
probResample n weighted gen
    | n <= 0    = return V.empty
    | otherwise = do
        -- let vweighted = V.fromList $ Foldable.toList weighted
        -- sorted <- mutableSortBy descendingOnFirst vweighted
        let lweighted = Foldable.toList weighted
            sorted    = sortProbs lweighted
        let probs     = F.scan (F.premap fst F.sum) sorted
            cdf       = V.fromList $ zip probs (fmap snd sorted)
        accumulateSample n cdf gen
  where
    accumulateSample
      :: PrimMonad m
      => Int -> Vector (Double, a) -> Gen (PrimState m) -> m (Vector a)
    accumulateSample size xs g = go V.empty size where
      go !acc s
        | s <= 0    = return $! acc
        | otherwise = do
            z <- uniform g
            let result = snd . fromJust . F.fold (F.find ((>= z) . fst)) $ xs -- FIXME fromJust
            go (V.cons result acc) (pred s)

probResampleIO :: Foldable f => Int -> f (Double, a) -> IO (Vector a)
probResampleIO n weighted = do
  gen <- createSystemRandom
  probResample n weighted gen


sortProbs :: Ord a => [(a, b)] -> [(a, b)]
sortProbs = hylo alg coalg where
  alg EmptyF      = []
  alg (LeafF x)   = [x]
  alg (NodeF l r) = mergeBy (compare `on` fst) l r

  coalg []  = EmptyF
  coalg [x] = LeafF x
  coalg xs  = NodeF l r where
    (l, r) = splitAt (length xs `div` 2) xs

  hylo :: Functor f => (f a -> a) -> (b -> f b) -> b -> a
  hylo f g = h where h = f . fmap h . g

data TreeF a r =
    EmptyF
  | LeafF a
  | NodeF r r
  deriving Functor

-- | Wrapper over the mutable sort process.
mutableSortBy :: PrimMonad m => V.Comparison a -> Vector a -> m (Vector a)
mutableSortBy comparison xs = do
  warm <- V.unsafeThaw xs
  V.sortBy comparison warm
  cool <- V.unsafeFreeze warm
  return $! cool

descendingOnFirst :: Ord a => (a, b) -> (a, b) -> Ordering
descendingOnFirst = flip compare `on` fst


probSample = undefined
probSampleIO = undefined


