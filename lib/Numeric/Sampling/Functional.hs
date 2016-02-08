{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE BangPatterns #-}

module Numeric.Sampling.Functional where

import qualified Control.Foldl as F
import Control.Monad.Primitive (PrimMonad, PrimState)
import qualified Data.Foldable as Foldable (toList)
import Data.Function (on)
import Data.List (sortBy)
import Data.Maybe (fromJust)
import System.Random.MWC

sortProbs :: (Foldable f, Ord a) => f (a, b) -> [(a, b)]
sortProbs = sortBy (compare `on` fst) . Foldable.toList

presample
  :: (PrimMonad m, Foldable f)
  => Int -> f (Double, a) -> Gen (PrimState m) -> m [a]
presample n weighted gen
    | n <= 0    = return []
    | otherwise = do
        let (bprobs, vals) = unzip $ sortProbs weighted
            probs          = drop 1 (F.scan F.sum bprobs)
            cumulative     = zip probs vals
        computeSample n cumulative gen
  where
    computeSample
      :: PrimMonad m => Int -> [(Double, a)] -> Gen (PrimState m) -> m [a]
    computeSample size xs g = go [] size where
      go !acc s
        | s <= 0    = return acc
        | otherwise = do
            z <- uniform g
            let (_, v) = fromJust $ F.fold (F.find ((>= z) . fst)) xs
            go (v:acc) (pred s)
{-# INLINABLE presample #-}

presampleIO :: Foldable f => Int -> f (Double, a) -> IO [a]
presampleIO n weighted = do
  gen <- createSystemRandom
  presample n weighted gen
{-# INLINABLE presampleIO #-}

