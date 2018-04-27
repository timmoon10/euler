module Main where

import NumToStr
import Data.List

main :: IO ()
main = putStrLn $ show answer

answer = foldl' (\sum n -> sum + countChars (numToStr n)) 0 [1..1000]
  where
    countChars :: String -> Int
    countChars [] = 0
    countChars (c : strTail) = case c of
                                 ' ' -> countChars strTail
                                 '-' -> countChars strTail
                                 ',' -> countChars strTail
                                 _   -> 1 + countChars strTail
