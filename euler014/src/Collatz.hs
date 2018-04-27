module Collatz
    ( collatzLength
    , cachedCollatzLength
    , newCollatzCache
    ) where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap

collatzLength n = fst $ cachedCollatzLength n newCollatzCache

naiveCollatzLength :: Int -> Int
naiveCollatzLength n | n <= 0 = 0
                     | n == 1 = 1
                     | even n = 1 + naiveCollatzLength (n `div` 2)
                     | otherwise = 1 + naiveCollatzLength (3*n + 1)

newCollatzCache :: IntMap Int
newCollatzCache = IntMap.singleton 1 1

cachedCollatzLength :: Int -> IntMap Int -> (Int, IntMap Int)
cachedCollatzLength n cache
  | n <= 0    = (0, cache)
  | otherwise = case IntMap.lookup n cache of
                  Just cachedLen -> (cachedLen, cache)
                  Nothing        -> (len, IntMap.insert n len nextCache)
  where
    (nextLen, nextCache) = if   even n
                           then cachedCollatzLength (n `div` 2) cache
                           else cachedCollatzLength (3*n + 1) cache
    len = nextLen + 1
