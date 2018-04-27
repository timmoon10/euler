module Main where

import LongNum
import Data.List

main :: IO ()
main = putStrLn $ show answer

answer = foldl' (+) 0 digits
  where
    digits = case fac 100 of
               Just d  -> d
               Nothing -> []

pow2 :: Int -> Maybe Digits
pow2 p
  | p < 0     = makeDigits 0
  | p == 0    = makeDigits 1
  | otherwise = do prevPow <- pow2 (p-1)
                   multiplyDigits prevPow 2

-- For Problem 20
fac :: Int -> Maybe Digits
fac n
  | n <= 1    = makeDigits 1
  | otherwise = do prevFac <- fac (n-1)
                   multiplyDigits prevFac n

-- For Problem 25
prob25 = n
  where (FibState n _ _) = firstFibStateWithLength 1000

fib :: Int -> Maybe Digits
fib n
  | n <= 2 = makeDigits 1
  | otherwise = do { fib1 <- fib (n-1)
                   ; fib2 <- fib (n-2)
                   ; addDigits fib1 fib2 }

data FibState = FibState Int Digits Digits
initFibState :: FibState
initFibState = case makeDigits 1 of
                 Just one -> FibState 2 one one
                 Nothing -> error "Could not construct initial Fibonacci state"
nextFibState :: FibState -> FibState
nextFibState (FibState n currFib prevFib)
  = case addDigits currFib prevFib of
      Just nextFib -> FibState (n+1) nextFib currFib
      Nothing -> error $ "Could not get " ++ (show (n+1)) ++ "th Fibonacci number"

firstFibStateWithLength :: Int -> FibState
firstFibStateWithLength l
  | l < 1     = error "Lengths must be positive"
  | l == 1    = initFibState
  | otherwise = getState $ firstFibStateWithLength (l-1)
    where getState (FibState n curr prev)
            = if   l <= (length curr)
              then (FibState n curr prev)
              else getState $ nextFibState (FibState n curr prev)
