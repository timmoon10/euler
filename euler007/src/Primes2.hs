module Primes2
    ( primeList
    , primesBelow
    , expandPrimeList
    , applySieve
    ) where

import Data.List

-- Get list of primes
primeList :: Int -> [Int]
primeList size = take size (computePrimes size ([], 0))
  where computePrimes n (primes, bound)
          = if (length primes) >= n
            then primes
            else computePrimes n (expandPrimeList (primes, bound) (2*bound))

-- Get all primes below a bound
primesBelow :: Int -> [Int]
primesBelow bound = fst $ expandPrimeList ([], 0) bound

-- Expand a list of primes
expandPrimeList :: ([Int], Int) -> Int -> ([Int], Int)
expandPrimeList (primes, bound) desiredBound
  | bound < 3 = expandPrimeList ([2,3], 3) desiredBound
  | bound == desiredBound = (primes, desiredBound)
  | bound > desiredBound = if   last primes <= desiredBound
                           then (primes, desiredBound)
                           else (filter (<=desiredBound) primes, desiredBound)
  | otherwise = expandPrimeList (primes ++ sievePrimes, sieveBound) desiredBound
  where
    sieveBound = min (bound^2) desiredBound
    sievePrimes = applySieve (bound+1) sieveBound primes

-- Apply sieve of Eratosthenes to range
applySieve start end primes = [start + i | i <- (elemIndices True finalSieve)]
  where
    multiples p = cycle $ True : (take (p-1) $ repeat False)
    apply p sieve = [ (sieve !! (i-start)) && (not (multiples p !! i))
                      | i <- [start..end]]
    initSieve = take (end - start + 1) $ repeat True
    finalSieve = (foldl (.) id [(\sieve -> apply p sieve) | p <- primes]) initSieve