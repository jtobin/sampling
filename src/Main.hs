{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Main where

import Numeric.Sampling
import qualified Data.Map.Strict as Map

main :: IO ()
main = do
  foo <- resampleIO 100 ([1..100000] :: [Int])
  print foo
