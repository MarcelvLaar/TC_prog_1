module Calendar where

import Data.Char
import ParseLib.Abstract
import Prelude hiding ((<$), ($>), (<*), (*>), sequence)
import DateTime


-- Exercise 6
data Calendar = Calendar{
    prodid :: String,
    version :: String,
    events :: [Event]}
    deriving (Eq, Ord, Show)

data Event = Event{
    dtstamp :: DateTime,
    uid :: String,
    dtstart :: DateTime,
    dtend :: DateTime,
    description :: Maybe String,
    summary :: Maybe String,
    location :: Maybe String}
    deriving (Eq, Ord, Show)

-- Exercise 7
data Token = Token {
    id :: String,
    rawData :: String
}
    deriving (Eq, Ord, Show)

scanCalendar :: Parser Char [Token]
scanCalendar =  greedy scanLine

isString :: Parser Char String
isString = greedy isStrOrSp

isStrOrSp :: Parser Char Char
isStrOrSp = satisfy isAlphaNum <|> symbol ' '

scanTest :: Parser Char Token
scanTest = Token <$> token "1" <*> token "2" 

scanLine :: Parser Char Token
scanLine =  (\a b -> (Token a b)) <$> isString <* symbol ':' <*> isString <* symbol '\n' <<|>
            (\a b -> (Token a b)) <$> isString <* symbol ':' <*> isString <* symbol '\n' <<|>
            (\a b -> (Token a b)) <$> isString <* symbol ':' <*> isString

testsl = parse scanCalendar "Testing:12315245\ntest2:testing\ndoesit:work\nihope:so\nhmm: I hope it works"

parseCalendar :: Parser Token Calendar
parseCalendar = undefined

recognizeCalendar :: String -> Maybe Calendar
recognizeCalendar s = run scanCalendar s >>= run parseCalendar

-- Exercise 8
printCalendar :: Calendar -> String
printCalendar = undefined
