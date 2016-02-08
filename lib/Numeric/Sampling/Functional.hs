{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}

module Numeric.Sampling.Functional (
    resample
  , resampleIO
  ) where

import qualified Control.Foldl as F
import Control.Monad.Primitive (PrimMonad, PrimState)
import qualified Data.Foldable as Foldable (toList)
import Data.Function (on)
import Data.List.Ordered (mergeBy)
import Data.Maybe (fromJust)
import System.Random.MWC

data TreeF a r =
    EmptyF
  | LeafF a
  | NodeF r r
  deriving Functor

sortProbs :: (Foldable f, Ord a) => f (a, b) -> [(a, b)]
sortProbs = hylo alg coalg . Foldable.toList where
  alg EmptyF      = []
  alg (LeafF x)   = [x]
  alg (NodeF l r) = mergeBy (compare `on` fst) l r

  coalg []  = EmptyF
  coalg [x] = LeafF x
  coalg xs  = NodeF l r where
    (l, r) = splitAt (length xs `div` 2) xs

  hylo :: Functor f => (f a -> a) -> (b -> f b) -> b -> a
  hylo f g = h where h = f . fmap h . g

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

presampleIO :: Foldable f => Int -> f (Double, a) -> IO [a]
presampleIO n weighted = do
  gen <- createSystemRandom
  presample n weighted gen

resample
  :: (PrimMonad m, Foldable f) => Int -> f a -> Gen (PrimState m) -> m [a]
resample n xs gen = do
  let len      = F.fold F.genericLength xs
      weighted = zip (repeat (1 / len)) (Foldable.toList xs)
  presample n weighted gen

resampleIO :: Foldable f => Int -> f a -> IO [a]
resampleIO n xs = do
  gen <- createSystemRandom
  resample n xs gen
