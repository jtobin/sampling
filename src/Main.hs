{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Main where

import Numeric.Sampling
import qualified Data.Map.Strict as Map

main :: IO ()
main = do
  -- foo <- resampleIO 100 ([1..1000000] :: [Int])
  foo <- resampleIO 100 (Map.fromList $ zip [1..1000] (repeat 'a'))
  print foo
