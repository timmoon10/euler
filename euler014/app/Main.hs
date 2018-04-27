module Main where

import Collatz
import Data.List
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap

main :: IO ()
main = putStrLn $ (show $ fst answer) ++ " has Collatz length of " ++ (show $ snd answer)

answer = (maxInd + 1, maxLength)
  where max = 999999
        cache = foldl' collatzCompute newCollatzCache [1..max]
        collatzCompute = (\c n -> snd $ cachedCollatzLength n c)
        collatzLengths = take max $ IntMap.elems cache
        maxLength = maximum collatzLengths
        Just maxInd = elemIndex maxLength collatzLengths
