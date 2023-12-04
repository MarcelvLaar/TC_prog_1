module Calendar where

import Data.Char
import ParseLib.Abstract
import Prelude hiding ((<$), ($>), (<*), (*>), sequence)
import DateTime
import Data.Maybe

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

--greedy parse as long as something isn't an \n or : 
isString :: Parser Char String
isString = greedy isStrOrSp

isStrOrSp :: Parser Char Char
isStrOrSp = satisfy (\c -> c /= '\n' && c /= ':')


--what this SHOULD be doing, is parse each multiline, combine the results of two multiline results and keep folding that until you can't anymore.
--the other two lines are in case there's no multiline or a next line available
scanLine :: Parser Char Token
scanLine =  Token <$> isString <* symbol ':' <*> ( (\a b -> a ++ b) <$> isString <*> (chainr (token "\n " *> isString) ((++) <$ succeed []))) <<|>
            Token <$> isString <* symbol ':' <*> isString <* symbol '\n' <<|>
            Token <$> isString <* symbol ':' <*> isString


parseCalendar :: Parser Token Calendar
parseCalendar = undefined

recognizeCalendar :: String -> Maybe Calendar
recognizeCalendar s = run scanCalendar s >>= run parseCalendar

-- Exercise 8
printCalendar :: Calendar -> String
printCalendar cal = "BEGIN:VCALENDAR\r\n" ++ prodIdStr (prodid cal) ++ versionStr (version cal) ++ eventsToStr (events cal) ++ "END:VCALENDAR"

--prodid to string
prodIdStr :: String -> String
prodIdStr a = "PRODID:" ++ a ++ "\r\n"

--version to string
versionStr :: String -> String
versionStr a = "VERSION:" ++ a ++ "\r\n"

--since we might have multiple events, perform event to string function on each element
eventsToStr :: [Event] -> String
eventsToStr [] = ""
eventsToStr (x:xs) = eventStr x ++ eventsToStr xs

--even to string
eventStr :: Event -> String
eventStr e = "BEGIN:VEVENT" ++ dtstampStr (dtstamp e) ++ uidStr (uid e) ++ dtstartStr (dtstart e) ++ dtendStr (dtend e) ++ descStr (description e) ++ sumStr (summary e) ++ locStr (location e) ++ "END:VEVENT"
 
 --dtstamp to string
dtstampStr :: DateTime -> String
dtstampStr dt = "DTSTAMP:" ++ printDateTime dt ++ "\r\n"

--uid to string
uidStr :: String -> String
uidStr s = "UID:" ++ s ++ "\r\n"

--dtstart to string
dtstartStr :: DateTime -> String
dtstartStr dt = "DTSTART:" ++ printDateTime dt ++ "\r\n"

--dtend to string
dtendStr :: DateTime -> String
dtendStr dt = "DTEND:" ++ printDateTime dt ++ "\r\n"

--description to string
descStr :: Maybe String -> String
descStr ma 
        | ma == Nothing = ""
        | otherwise = "DESCRIPTION:" ++ fromMaybe "" ma ++ "\r\n"

--summary to string
sumStr :: Maybe String -> String
sumStr ma 
        | ma == Nothing = ""
        | otherwise = "SUMMARY:" ++ fromMaybe "" ma ++ "\r\n"

--location to string
locStr :: Maybe String -> String
locStr ma 
        | ma == Nothing = ""
        | otherwise = "LOCATION:" ++ fromMaybe "" ma ++ "\r\n"


