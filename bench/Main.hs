{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Main where

import Criterion.Main
import Numeric.Sampling (sampleIO, resampleIO)

setupEnv :: IO [Int]
setupEnv = return [1..1000000]

main :: IO ()
main = defaultMain [
  env setupEnv $ \x ->
      bgroup "small" [
        bench "sample" $ nfIO (sampleIO 100 x)
      , bench "resample" $ nfIO (resampleIO 100 x)
      ]
    ]

