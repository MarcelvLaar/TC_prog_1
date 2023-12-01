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

isStrOrSp :: Parser Char Char --dit moet beter kunnen, toch? misschien anySymbol?
isStrOrSp = satisfy isAlphaNum <|> symbol ' ' <|> symbol '.' <|> symbol '/' <|> symbol '-' <|> symbol '@'

scanTest :: Parser Char Token
scanTest = Token <$> token "1" <*> token "2"

scanLine :: Parser Char Token
-- scanLine =  Token <$> isString <* symbol ':' <*> isString <* symbol '\n' <<|>
--             Token <$> isString <* symbol ':' <*> isString <* symbol '\n' <<|>
--             Token <$> isString <* symbol ':' <*> isString
scanLine =  Token <$> isString <* symbol ':' <*> isString <* symbol '\n' <<|>
            Token <$> isString <* symbol ':' <*> isString 

sl_test_1 = run scanCalendar "Testing:12315245\ntest2:testing\ndoesit:work\nihope:so\nhmm: I hope it works"
sl_test_2 = run scanCalendar "BEGIN:VCALENDAR\n PRODID:-//hacksw/handcal//NONSGML v1.0//EN\n VERSION:2.0\n BEGIN:VEVENT\n SUMMARY:Bastille Day Party\n UID:19970610T172345Z-AF23B2@example.com\n DTSTAMP:19970610T172345Z\n DTSTART:19970714T170000Z\n DTEND:19970715T040000Z\n END:VEVENT\n END:VCALENDAR"

parseCalendar :: Parser Token Calendar
parseCalendar = undefined

recognizeCalendar :: String -> Maybe Calendar
recognizeCalendar s = run scanCalendar s >>= run parseCalendar

-- Exercise 8
printCalendar :: Calendar -> String
printCalendar = undefined
