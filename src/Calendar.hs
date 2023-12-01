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
isStrOrSp = satisfy (\c -> c /= '\n' && c /= ':')

newLineAllowed :: Parser Char String
newLineAllowed = many (satisfy (/= ':'))

scanLine :: Parser Char Token
scanLine = Token <$> isString <* symbol ':' <*> isString <* symbol '\n' <<|>
            Token <$> isString <* symbol ':' <*> isString <<|>
            Token <$> newLineAllowed <* symbol ':' <*> isString <* symbol '\n'
            -- Token <$> isString <* symbol ':' <*> newLineAllowed <* symbol '\n'



sl_test_1 = run scanCalendar "Testing:12315245\ntest2:testing\ndoesit:work\nihope:so\nhmm: I hope it works"
sl_test_2 = run scanCalendar "BEGIN:VCALENDAR\n PRODID:-//hacksw/handcal//NONSGML v1.0//EN\n VERSION:2.0\n BEGIN:VEVENT\n SUMMARY:Bastille Day Party\n UID:19970610T172345Z-AF23B2@example.com\n DTSTAMP:19970610T172345Z\n DTSTART:19970714T170000Z\n DTEND:19970715T040000Z\n END:VEVENT\n END:VCALENDAR"
sl_test_3 = run scanCalendar "BEGIN:VCALENDAR\n PRODID:-//hacksw/handcal//NONSGML v1.0//EN\n VERSION:2.0\n BEGIN:VEVENT\n SUMMARY:Bastille \nDay\n Party\n UID:19970610T172345Z-AF23B2@example.com\n DTSTAMP:19970610T172345Z\n DTSTART:19970714T170000Z\n DTEND:19970715T040000Z\n END:VEVENT\n END:VCALENDAR"
sl_test_4 = run scanCalendar "BEGIN:VCALENDAR\nVERSION:2.0\nPRODID:-//hacksw/handcal//NONSGML v1.0//EN\nBEGIN:VEVENT\nUID:12345@example.com\nDTSTAMP:20111205T170000Z\nDTSTART:20111205T170000Z\nDTEND:20111205T210000Z\nSUMMARY:This is a very long description th\n at spans over multiple lines.\nEND:VEVENT\nEND:VCALENDAR"
sl_test_5 = run scanCalendar "BEGIN:VCALENDAR\n PRODID:-//hacksw/handcal//NONSGML v1.0//EN\n VERSION:2.0\n BEGIN:VEVENT\n SUMMARY:Bastille Day Party\n UID:19970610T172345Z-AF23B2@example.com\n DTSTAMP:19970610T172345Z\n DTSTART:19970714T170000Z\n DTEND:19970715T040000Z\n END:VEVENT\n END:VCALENDAR"

checkToken :: String -> Token -> Bool --checks id of token against something
checkToken s (Token id rawdata) = s == id

parseBegin :: Parser Token Token
parseBegin = satisfy (checkToken "BEGIN")

parseEnd :: Parser Token Token
parseEnd = satisfy (checkToken "END")

parseObject :: Parser Token a -> Parser Token a
parseObject o = pack parseBegin o parseEnd

parseCalendar :: Parser Token Calendar
parseCalendar = undefined

recognizeCalendar :: String -> Maybe Calendar
recognizeCalendar s = run scanCalendar s >>= run parseCalendar

-- Exercise 8
printCalendar :: Calendar -> String
printCalendar = undefined

  --
tests :: Parser Char (String,String)
tests = (,) <$> many anySymbol <* symbol ',' <*> greedy1 anySymbol

tests' :: Parser Char (String, String)
tests' = (,) <$> greedy1 anySymbol <* symbol ',' <*> greedy1 anySymbol

testints = parse tests "12,34"
testintz = parse tests' "12,34"
