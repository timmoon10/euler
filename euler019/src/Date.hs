module Date
    ( Month(..)
    , isLeapYear
    , monthLength
    , yearLength
    , nextMonth
    , prevMonth
    , makeDate
    , unixDate
    , DayOfWeek(..)
    , dateToDayOfWeek
    ) where

import Data.List
import Data.Maybe

------------------------------------------------
-- Month type
------------------------------------------------
data Month = January   | February | March    | April
           | May       | June     | July     | August
           | September | October  | November | December
  deriving (Eq, Ord, Show, Read, Bounded, Enum)

-- Whether a year is a leap year
isLeapYear :: Int -> Bool
isLeapYear year
  | year `rem` 4   /= 0 = False
  | year `rem` 100 /= 0 = True
  | year `rem` 400 /= 0 = False
  | otherwise           = True

-- Month length
{- Thirty days has September,
   April, June and November.
   All the rest have thirty-one,
   Saving February alone,
   Which has twenty-eight, rain or shine.
   And on leap years, twenty-nine.
-}
monthLength :: (Month, Int) -> Int
monthLength (month, year)
  | month `elem` [September, April, June, November] = 30
  | month == February = if isLeapYear year then 29 else 28
  | otherwise = 31

-- Month arithmetic
nextMonth    :: Month -> Month
prevMonth    :: Month -> Month
monthToIndex :: Month -> Int
indexToMonth :: Int -> Month
shiftMonth   :: Month -> Int -> Month
nextMonth month = shiftMonth month 1
prevMonth month = shiftMonth month (-1)
monthToIndex month = fromMaybe 0 (elemIndex month [January ..])
indexToMonth index = [January ..] !! (index `mod` 12)
shiftMonth month shift = indexToMonth (monthToIndex month + shift)

------------------------------------------------
-- Date type (month-day-year format)
------------------------------------------------
data Date = Date Month Int Int
instance Show Date where
  show (Date month day year) = show month
                               ++ " " ++ show day
                               ++ ", " ++ show year

-- Constructor
makeDate :: Month -> Int -> Int -> Date
makeDate month day year
  | year < 1582  = error "Invalid year (the Gregorian calendar was introduced in 1582)"
  | day < 1 || day > monthLength (month, year) = error "Invalid day"
  | otherwise = Date month day year

-- Year length
yearLength :: Int -> Int
yearLength year = if (isLeapYear year) then 366 else 365

-- Convert month-day-year date to number of days since Unix epoch
unixDate :: Date -> Int
unixDate (Date January 1 year)
  | year == 1970 = 0
  | year <  1970 = unixDate (Date January 1 (year+1)) - yearLength year
  | year >  1970 = unixDate (Date January 1 (year-1)) + yearLength (year-1)
unixDate (Date month 1 year) = unixDate (Date pmonth 1 year)
                               + monthLength (pmonth, year)
  where pmonth = prevMonth month
unixDate (Date month day year) = unixDate (Date month 1 year) + day - 1

------------------------------------------------
-- Day of week type
------------------------------------------------
data DayOfWeek = Sunday   | Monday | Tuesday  | Wednesday
               | Thursday | Friday | Saturday
  deriving (Eq, Ord, Show, Read, Bounded, Enum)

-- Day of week arithmetic
dayOfWeekToIndex :: DayOfWeek -> Int
indexToDayOfWeek :: Int -> DayOfWeek
shiftDayOfWeek   :: DayOfWeek -> Int -> DayOfWeek
dayOfWeekToIndex day = fromMaybe 0 (elemIndex day [Sunday ..])
indexToDayOfWeek index = [Sunday ..] !! (index `mod` 7)
shiftDayOfWeek day shift = indexToDayOfWeek (dayOfWeekToIndex day + shift)

-- Get a date's day of week
dateToDayOfWeek :: Date -> DayOfWeek
dateToDayOfWeek date = shiftDayOfWeek refDayOfWeek daysSinceRef
  where refDate = Date January 1 1900
        refDayOfWeek = Monday
        daysSinceRef = unixDate date - unixDate refDate
