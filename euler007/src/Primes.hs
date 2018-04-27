module Primes
    ( primes
    , expand_primes
    , prime_list
    ) where

-- Get a number of primes
prime_list :: Int -> [Int]
prime_list num
  | num <= 1  = [2]
  | otherwise = take num (compute_primes num [2,3])
  where
    compute_primes n primes = if (length primes) >= n
      then primes
      else compute_primes n (expand_primes primes (2 * last primes))

-- Get primes below a number
primes :: Int -> [Int]
primes max
  | max < 2   = []
  | max == 2  = [2]
  | otherwise = expand_primes [2,3] max

-- Expand a list of primes to get all primes below a number
expand_primes :: [Int] -> Int -> [Int]
expand_primes primes max
  | primes == []        = expand_primes [2,3] max
  | last_prime >= max   = primes
  | last_prime^2 >= max = new_primes
  | otherwise           = expand_primes new_primes max
  where
    last_prime = last primes
    start = last_prime + 1
    end = min (last_prime^2) max
    size = end - start + 1
    sieve_list = sieve primes start (replicate size True)
    new_primes = primes ++ (parse_sieve start sieve_list)

-- Sieve of Eratosthenes
sieve :: [Int] -> Int -> [Bool] -> [Bool]
sieve primes start candidates
  | primes == [] = candidates
  | otherwise    = sieve (tail primes) start next_candidates
  where
    next_candidates = sieve_step (head primes) start candidates

-- Apply step of sieve of Eratosthenes
-- Remove multiples of p from a list of candidate numbers
sieve_step :: Int -> Int -> [Bool] -> [Bool]
sieve_step p start candidates
  | p == 0                   = error "sieve_step: p must be non-zero"
  | pos >= length candidates = candidates
  | otherwise                = before ++ [False] ++ next
  where
    pos = (-start) `mod` (abs p)
    (before, current) = splitAt pos candidates
    next = sieve_step p 1 (tail current)

-- Parse results from sieve of Eratosthenes
parse_sieve :: Int -> [Bool] -> [Int]
parse_sieve start candidates
  | candidates == [] = []
  | head candidates  = start : tail_nums
  | otherwise        = tail_nums
  where
    tail_nums = parse_sieve (start+1) (tail candidates)
