module Main where

import Date

main :: IO ()
main = putStrLn (show answer)

answer = length $ filter (== Sunday) firstsOfTheMonth
  where firstsOfTheMonth = [dateToDayOfWeek (makeDate month day year)
                             | month <- [January .. December]
                             , day   <- [1]
                             , year  <- [1901..2000]]
                           