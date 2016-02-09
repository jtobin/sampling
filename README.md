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

Sample ten elements from a list, without replacement:

    > sampleIO 10 ['a'..'z']
    Just "azctumlhwj"

Sample five elements from a Map, with replacement:

    > import qualified Data.Map.Strict as Map
    > sampleIO 5 (Map.fromList [(1, "apple"), (2, "orange"), (3, "pear")])
    ["apple", "apple", "pear", "orange", "pear"]

## Development

On the todo list:

* Performance improvements
* A *psample* function to go with *presample*

## Etc.

PRs and issues welcome.
