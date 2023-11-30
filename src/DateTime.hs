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

--wat testen met parsers

greedyNatural :: Parser Char Int
greedyNatural = foldl (\ a b -> a * 10 + b) 0 <$> greedy1 newdigit

coolInteger :: Parser Char Int
coolInteger = (negate <$ symbol '-') `option` id  <*>  greedyNatural

ints :: Parser Char (Int, Int)
ints = (,) <$> integer <* symbol ',' <*> integer
-- ints = (\a b -> (a, b)) <$> coolInteger <* symbol ',' <*> coolInteger

-- Exercise 1

parseDateTime :: Parser Char DateTime
parseDateTime = DateTime <$> parseDate <* anySymbol <*> parseTime <*> parseUTC

parseUTC :: Parser Char Bool
parseUTC = (True <$ symbol 'Z') <<|> (False <$ epsilon)
-- parseUTC = const True <$> (symbol 'Z') <<|> (const False <$> epsilon)


parseTime :: Parser Char Time
parseTime = Time <$> parseHour <*> parseMinute <*> parseSecond

parseHour :: Parser Char Hour
parseHour = Hour <$> parseDigits 2

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

parseDigits :: Int -> Parser Char Int
parseDigits n = foldl (\ a b -> a * 10 + b) 0 <$> replicateM n newdigit


-- Exercise 2
run :: Parser b a -> [b] -> Maybe a
run parser l = checking (parse parser l)

checking :: [(a, [b])] -> Maybe a
checking [(a, [])] = Just a
checking [(a, _)] = Nothing
checking [] = Nothing

-- Exercise 3
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
-- parsePrint s = fmap printDateTime $ run parseDateTime s

-- Exercise 5
checkDateTime :: DateTime -> Bool
checkDateTime (DateTime (Date (Year year) (Month month) (Day day)) (Time (Hour hour) (Minute minute) (Second second)) _) = month <= 12 &&
                    checkDay year month day &&
                    hour < 24 &&
                    minute < 60 &&
                    second < 60

-- Year -> Month -> Day -> Bool
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

checkLeapYear :: Int -> Int -> Bool
checkLeapYear y d
                | y `mod` 4 == 0 && (((y + 100) `mod` 200) /= 0) && (((y + 200) `mod` 400) /= 0) = d <= 29
                |otherwise = d <= 28

-- Own testing functions

fromMaybe :: Maybe a -> a
fromMaybe Nothing = error "something went wrong"
fromMaybe (Just x) = x --this is just a prelude function, no clue why it's not available

toMaybe :: a -> Maybe a
toMaybe a = Just a

ex3test :: String
ex3test = printDateTime $ fromMaybe $ run parseDateTime "19970610T172345Z"

-- ex3test2 :: Maybe String
-- ex3test2 = run parseDateTime "19970715T040000Z"

ex4test :: Maybe String
ex4test = parsePrint "19970715T040000Z"
