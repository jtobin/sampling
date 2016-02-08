{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Main where

import Numeric.Sampling

main :: IO ()
main = do
  foo <- resampleIO 100 ([1..1000000] :: [Int])
  -- foo <- resampleIO 100 [1..1000000]
  print foo
