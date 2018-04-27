module Lib
    ( answer
    , first_divisible_triang
    , num_divisors
    ) where

answer :: Int
answer = first_divisible_triang 500

-- First triangular number with a given number of divisors
first_divisible_triang :: Int -> Int
first_divisible_triang goal = test 1
  where
    test i
      | (num_divisors candidate) > goal = candidate
      | otherwise                       = test (i+1)
      where candidate = triang i
    

-- nth triangular number
triang :: Int -> Int
triang n = (n * (n + 1)) `div` 2

-- Number of divisors
num_divisors :: Int -> Int
num_divisors n = num_divisors (n, 2)
  where
    num_divisors (i, guess)
      | i <= guess = 1
      | otherwise  = (f+1) * num_divisors (i `div` (p^f), p+1)
      where
        p = prime_divisor i guess
        f = factor_exponent i p
    factor_exponent i p
      | i `mod` p /= 0  = 0
      | otherwise       = 1 + factor_exponent (i `div` p) p
    prime_divisor i guess
      | i `mod` guess == 0 = guess
      | otherwise          = prime_divisor i (guess+1)
