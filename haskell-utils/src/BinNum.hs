module BinNum
    ( BinNum(..)
    ) where

-- Binary represenation of integer
-- The binary digits are ordered in increasing significance, with an
-- implicit most-significant-digit of one.
data BinNum = Zero | Positive [Bool] | Negative [Bool]

instance Show BinNum where
  show Zero = "0"
  show (Positive []) = "1"
  show (Positive (unit : next)) = show (Positive next) ++ if unit then "1" else "0"
  show (Negative b) = "-" ++ show (Positive b)

instance Eq BinNum where
  Zero == Zero = True
  (Positive x) == (Positive y) = x == y
  (Negative x) == (Negative y) = x == y
  x == y = False

instance Ord BinNum where
  compare Zero Zero = EQ
  compare (Positive x) (Positive y) = binCompare x y
    where binCompare a b = case (a, b) of
                             ([], []) -> EQ
                             (_, [])  -> GT
                             ([], _)  -> LT
                             ([False], [False]) -> EQ
                             ([False], [True])  -> LT
                             ([True] , [False]) -> GT
                             ([True] , [True])  -> EQ
                             (c, d) -> binCompare (tail c) (tail d)
  compare (Negative x) (Negative y) = compare (Positive y) (Positive x)
  compare x y = case (x, y) of (Positive _, _) -> GT
                               (_, Positive _) -> LT
                               (Negative _, _) -> LT
                               (_, Negative _) -> GT

instance Num BinNum where

  -- Addition
  x + Zero = x
  Zero + y = y
  (Positive x) + (Positive y) = Positive $ binSum x y False
  (Negative x) + (Negative y) = negate $ (Positive x) + (Positive y)
  (Positive x) + (Negative y) = (Positive x) - (Positive y)
  (Negative x) + (Positive y) = (Positive y) - (Positive x)

  -- Subtraction
  x - Zero = x
  Zero - y = negate y
  (Positive x) - (Positive y) = case compare (Positive x) (Positive y) of
                                  EQ -> Zero
                                  GT -> Positive $ binDiff x y False
                                  LT -> negate $ (Positive y) - (Positive x)
  (Negative x) - (Negative y) = (Positive y) - (Positive x)
  (Positive x) - (Negative y) = (Positive x) + (Positive y)
  (Negative x) - (Positive y) = negate $ (Positive x) + (Positive y)

  -- Multiplication
  x * Zero = Zero
  Zero * x = Zero
  x * (Positive []) = x
  (Positive []) * y = y
  (Positive x) * (Positive y) = if   (Positive x) >= (Positive y)
                                then Positive $ binProd x y
                                else Positive $ binProd y x
  (Negative x) * (Negative y) = (Positive x) * (Positive y)
  (Negative x) * (Positive y) = negate $ (Positive x) * (Positive y)
  (Positive x) * (Negative y) = negate $ (Positive x) * (Positive y)
  
  -- Integer to binary number
  fromInteger n | n == 0    = Zero
                | n > 0     = Positive $ intToBin n
                | otherwise = negate $ fromInteger (-n)
    where intToBin 1 = []
          intToBin n = (1 == n `mod` 2) : intToBin (n `div` 2)

  -- Additional arithmetic functions
  negate x = case x of Zero       -> Zero
                       Positive b -> Negative b
                       Negative b -> Positive b
  abs x = case x of Negative b -> Positive b
                    _          -> x
  signum x = case x of Zero       -> Zero
                       Positive _ -> Positive []
                       Negative _ -> Negative []

-- Convert binary number to integer type
binToInt :: BinNum -> Maybe Int
binToInt Zero = Just 0
binToInt (Positive []) = Just 1
binToInt (Positive x) = case binToInt (Positive (tail x)) of
                          Nothing -> Nothing
                          Just prev -> if   prev < bound
                                       then if head x
                                            then Just (2 * prev + 1)
                                            else Just (2 * prev)
                                       else Nothing
  where bound = ((maxBound :: Int) - 1) `div` 2
binToInt (Negative []) = Just (-1)
binToInt (Negative x) = case binToInt (Negative (tail x)) of
                          Nothing -> Nothing
                          Just prev -> if   prev > bound
                                       then if head x
                                            then Just (2 * prev - 1)
                                            else Just (2 * prev)
                                       else Nothing
  where bound = ((minBound :: Int) + 1) `quot` 2

-- Exclusive-or
xor :: Bool -> Bool -> Bool
xor True  False = True
xor False True  = True
xor False False = False
xor True  True  = False

-- Compute sum of two positive binary numbers
-- 'carry` indicates whether a one is carried over from the previous
-- addition step
binSum :: [Bool] -> [Bool] -> Bool -> [Bool]
binSum [] [] carry = [carry]
binSum x [] True = (head x) : binSum (tail x) [] False
binSum x [] False = if head x
                    then False : binSum (tail x) [] False
                    else True : tail x
binSum [] y carry = binSum y [] carry
binSum x y carry = unit : binSum (tail x) (tail y) tailCarry
  where xUnit = head x
        yUnit = head y
        unit = xUnit `xor` yUnit `xor` carry
        tailCarry = (xUnit && yUnit) || (yUnit && carry) || (carry && xUnit)

-- Compute difference of two positive binary numbers
-- We require x > y. 'carry` indicates whether a one is carried over
-- from the previous subtraction step
binDiff :: [Bool] -> [Bool] -> Bool -> [Bool]
binDiff x [] True = (head x) : binDiff (tail x) [] False
binDiff x [] False = if   head x
                     then False : tail x
                     else True : binDiff (tail x) [] False
binDiff x y carry = unit : binDiff (tail x) (tail y) tailCarry
  where xUnit = head x
        yUnit = head y
        unit = (xUnit && yUnit && carry) || ((not xUnit) && (yUnit `xor` carry))
        tailCarry = (xUnit && yUnit && carry) || ((not xUnit) && (yUnit || carry))

-- Compute product of two positive binary numbers
binProd :: [Bool] -> [Bool] -> [Bool]
binProd x [] = x
binProd x y = if head y
              then binSum x (False: binProd x (tail y)) False
              else False: binProd x (tail y)
