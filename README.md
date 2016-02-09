# sampling

[![Build Status](https://secure.travis-ci.org/jtobin/sampling.png)](http://travis-ci.org/jtobin/sampling)
[![Hackage Version](https://img.shields.io/hackage/v/sampling.svg)](http://hackage.haskell.org/package/sampling)

Basic sampling functionality.

Exports variations on two simple functions for sampling from arbitrary
'Foldable' collections:

* *sample*, for sampling without replacement
* *resample*, for sampling with replacement (i.e. a bootstrap)

## Usage

*sampling* uses the PRNG provided by
[mwc-random](https://hackage.haskell.org/package/mwc-random) for randomness.
You can either provide a generator for functions that require one, e.g.:

    > import Numeric.Sampling
    > gen <- createSystemRandom
    > resample 100 [1..1000] gen

Or simply use the `IO`-specialized versions that will use the system's source
of randomness:

    > resampleIO 100 [1..1000]

The non-`IO` specialized functions can be used with any `PrimMonad`.

## Examples

Sample ten elements from a list, with replacement:

    > resampleIO 10 ['a'..'g']
    "ddgaefbgef"

Sample five elements from a Map, without replacement:

    > import qualified Data.Map.Strict as Map
    > sampleIO 5 (Map.fromList (zip [1..1000] (scanl1 (/) [1..])))
    Just [0.0,1.0126536951759521e-203,2.9893108271424046e-50,0.0,0.0]

## Etc.

PRs and issues welcome.
