module NumToStr
    ( numToStr
    ) where

-- Convert a number to a British English string
numToStr :: Int -> String
numToStr n
  | n < 0               = "negative " ++ (numToStr (-n))
  | n < 1000            = parseUnderThousand n
  | scaleInd >= 8       = show n
  | scaleRemainder == 0 = scaleName
  | otherwise           = scaleName ++ ", " ++ (numToStr scaleRemainder)
  where
    scaleNames = [ "zero"       , "thousand"
                 , "million"    , "billion"
                 , "trillion"   , "quadrillion"
                 , "quintillion", "sextillion"
                 ]
    log1000 x = if x < 1000 then 0 else 1 + log1000 (x `div` 1000)
    pow1000 x = if x <= 0 then 1 else 1000 * pow1000 (x - 1)
    scaleInd = log1000 n
    scale = pow1000 scaleInd
    scaleMultiples = n `div` scale
    scaleRemainder = n `mod` scale
    scaleName = (parseUnderThousand scaleMultiples) ++ " "
                ++ (scaleNames !! scaleInd)

-- Helper functions to parse thousands, tens, and units places
parseUnderThousand :: Int -> String
parseUnderHundred  :: Int -> String
parseUnderTwenty   :: Int -> String
parseUnderThousand n
  | n < 0 || n >= 1000 = numToStr n
  | n < 100            = parseUnderHundred n
  | remainder == 0     = hundredsName
  | otherwise          = hundredsName ++ " and "
                         ++ (parseUnderHundred remainder)
  where
    hundreds = n `div` 100
    hundredsName = (parseUnderTwenty hundreds) ++ " hundred"
    remainder = n `mod` 100
parseUnderHundred n
  | n < 0 || n >= 100 = numToStr n
  | n < 20            = parseUnderTwenty n
  | units == 0        = tensName
  | otherwise         = tensName ++ "-" ++ (parseUnderTwenty units)
  where
    tensNames = [ "zero"  , "ten"  , "twenty", "thirty"
                , "forty" , "fifty", "sixty" , "seventy"
                , "eighty", "ninety"
                ]
    tensName  = tensNames !! (n `div` 10)
    units = n `mod` 10
parseUnderTwenty n
  | n < 0 || n >= 20 = numToStr n
  | otherwise = numberNames !! n
  where
    numberNames = [ "zero"   , "one"      , "two"     , "three"
                  , "four"   , "five"     , "six"     , "seven"
                  , "eight"  , "nine"     , "ten"     , "eleven"
                  , "twelve" , "thirteen" , "fourteen", "fifteen"
                  , "sixteen", "seventeen", "eighteen", "nineteen"
                  ]
