module Main where

import Primes2

main :: IO ()
main = putStrLn (show answer)

answer = last $ primeList 10001
