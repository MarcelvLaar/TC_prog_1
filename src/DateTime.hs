module DateTime where

import ParseLib.Abstract
import Prelude hiding ((<$), ($>), (<*), (*>), sequence)
import Data.Type.Coercion (TestCoercion(testCoercion))
import Control.Monad (replicateM, replicateM_)
import Data.Sequence (replicateA, Seq (Empty))
import Utils.Containers.Internal.StrictPair (StrictPair)

-- | "Target" datatype for the DateTime parser, i.e, the parser should produce elements of this type.
data DateTime = DateTime { date :: Date
                         , time :: Time
                         , utc  :: Bool }
    deriving (Eq, Ord, Show)

data Date = Date { year  :: Year
                 , month :: Month
                 , day   :: Day }
    deriving (Eq, Ord, Show)

newtype Year  = Year  { runYear  :: Int } deriving (Eq, Ord, Show)
newtype Month = Month { runMonth :: Int } deriving (Eq, Ord, Show)
newtype Day   = Day   { runDay   :: Int } deriving (Eq, Ord, Show)

data Time = Time { hour   :: Hour
                 , minute :: Minute
                 , second :: Second }
    deriving (Eq, Ord, Show)

newtype Hour   = Hour   { runHour   :: Int } deriving (Eq, Ord, Show)
newtype Minute = Minute { runMinute :: Int } deriving (Eq, Ord, Show)
newtype Second = Second { runSecond :: Int } deriving (Eq, Ord, Show)


-- Exercise 1

--
parseDateTime :: Parser Char DateTime
parseDateTime = DateTime <$> parseDate <* anySymbol <*> parseTime <*> parseUTC

--parsing utc. parses True if there is a Z at the end of a date string
parseUTC :: Parser Char Bool
parseUTC = (True <$ symbol 'Z') <<|> (False <$ epsilon)

parseTime :: Parser Char Time
parseTime = Time <$> parseHour <*> parseMinute <*> parseSecond

--parses 2 digits
parseHour :: Parser Char Hour
parseHour = Hour <$> parseDigits 2

--parses 2 digits
parseMinute :: Parser Char Minute
parseMinute = Minute <$> parseDigits 2

parseSecond :: Parser Char Second
parseSecond = Second <$> parseDigits 2

parseDate :: Parser Char Date
parseDate = Date <$> parseYear <*> parseMonth <*> parseDay

parseDay :: Parser Char Day
parseDay = Day <$> parseDigits 2

parseMonth :: Parser Char Month
parseMonth = Month <$> parseDigits 2

parseYear :: Parser Char Year
parseYear = Year <$> parseDigits 4

--parse digit n times and combine the values by multiplying the previous result times 10
parseDigits :: Int -> Parser Char Int
parseDigits n = foldl (\ a b -> a * 10 + b) 0 <$> replicateM n newdigit


-- Exercise 2
run :: Parser b a -> [b] -> Maybe a
run parser l = checking (parse parser l)

--we check whether the parsed string returned a result with no remainder. 
--if it did, return the result. if not, Nothing
checking :: [(a, [b])] -> Maybe a
checking [(a, [])] = Just a
checking [(a, _)] = Nothing
checking [] = Nothing

-- Exercise 3
--print date, time and utc that's stored in the DateTime. also adding the separator "T"
printDateTime :: DateTime -> String
printDateTime (DateTime d t u ) = printDate d ++ "T" ++ printTime t ++ printUTC u

printDate :: Date -> String
printDate (Date (Year y) (Month m) (Day d)) = intShow 4 y ++ intShow 2 m ++ intShow 2 d

printTime :: Time -> String
printTime (Time (Hour h) (Minute m) (Second s)) = intShow 2 h ++ intShow 2 m ++ intShow 2 s

printUTC :: Bool -> String
printUTC True = "Z"
printUTC False = []

intShow :: Int -> Int -> String -- 4 0 -> 0000
intShow l i = concat (replicate (l - length (show i)) "0") ++ show i


-- Exercise 4
parsePrint :: [Char] -> Maybe String
parsePrint s = printDateTime <$> run parseDateTime s

-- Exercise 5
--checks for month, day, hour, minute and second if they're valid numbers
checkDateTime :: DateTime -> Bool
checkDateTime (DateTime (Date (Year year) (Month month) (Day day)) (Time (Hour hour) (Minute minute) (Second second)) _) = month <= 12 &&
                    checkDay year month day &&
                    hour < 24 &&
                    minute < 60 &&
                    second < 60

-- Year -> Month -> Day -> Bool
--we check for each given month if the day accompanying it is valid or not. for february, we also care about leapyear
--this also immediately checks whether month is valid or not
checkDay :: Int -> Int -> Int -> Bool
checkDay y m d = case m of
                    1 -> d <= 31
                    2 -> checkLeapYear y d
                    3 -> d <= 31
                    4 -> d <= 30
                    5 -> d <= 31
                    6 -> d <= 30
                    7 -> d <= 31
                    8 -> d <= 31
                    9 -> d <= 30
                    10 -> d <= 31
                    11 -> d <= 30
                    12 -> d <= 31
                    otherwise -> False

--function to calculate whether a year is a leap year or not and if so, allow 29 days for february. if not, 28
checkLeapYear :: Int -> Int -> Bool
checkLeapYear y d
                | y `mod` 4 == 0 && (((y + 100) `mod` 200) /= 0) && (((y + 200) `mod` 400) /= 0) = d <= 29
                |otherwise = d <= 28


