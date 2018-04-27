module LongNum
    ( makeDigits
    , stringToDigits
    , digitsToString
    , addDigits
    ) where

import Data.Char

type Digits = [Int]

-- Construct digit list from non-negative integer
makeDigits :: Int -> Maybe Digits
makeDigits i = cleanDigits [i]

-- Construct string to digit list
stringToDigits :: String -> Maybe Digits
stringToDigits str = cleanDigits $ reverse $ rawParse str
  where
    rawParse [] = []
    rawParse (s : sTail) = (read [s]) : (rawParse sTail)

-- Convert digit list to string
digitsToString :: Digits -> String
digitsToString digit = reverse $ rawParse digit
  where
    rawParse [] = []
    rawParse (d : dTail) = (intToDigit d) : (rawParse dTail)

-- Put digit list in order
-- Digit list corresponds to a non-negative number
cleanDigits :: Digits -> Maybe Digits
cleanDigits [] = Just []
cleanDigits [d]
  | d < 0  = Nothing
  | d == 0 = Just []
  | d > 0  = do cleanedTail <- cleanDigits [d `div` 10]
                Just $ (d `mod` 10) : cleanedTail
cleanDigits (d : dTail)
  | d < 0     = Nothing
  | otherwise = do cleanedTail <- cleanDigits correctedTail
                   if cleanedDigit == 0 && cleanedTail == []
                   then Just []
                   else Just $ cleanedDigit : cleanedTail
    where
      cleanedDigit = d `mod` 10
      correctedTail = (head dTail + d `div` 10) : (tail dTail)

-- Add two digit lists
addDigits :: Digits -> Digits -> Maybe Digits
addDigits x y = cleanDigits $ rawAddDigits x y
  where
    rawAddDigits (a : aTail) (b : bTail) = (a + b) : (rawAddDigits aTail bTail)
    rawAddDigits a [] = a
    rawAddDigits a b = rawAddDigits b a
