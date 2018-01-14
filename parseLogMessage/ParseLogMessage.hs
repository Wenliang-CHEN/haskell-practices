module ParseLog where
import Data.List
import Data.List.Split
import Log

data MessageType = Info
    | Warning
    | Error Int
    deriving (Show, Eq) 

type TimeStamp = Int

data LogMessage = LogMessage MessageType TimeStamp String
    | Unknown String
    deriving (Show, Eq)

buildMessage :: [String] -> String
buildMessage [x] = x
buildMessage (x:xs) = x ++ " " ++ (buildMessage xs)

getElement :: [String] -> Int -> String
getElement list position = list!!position

toInt :: String -> Int
toInt ele = read ele::Int

getError :: String -> MessageType
getError servLv = Error (toInt servLv)

makeErrorMessage :: [String] -> LogMessage
makeErrorMessage elements = LogMessage (getError $ getElement elements 0) (toInt $ getElement elements 1) (buildMessage $ drop 2 elements)

makeInfoMessage :: [String] -> LogMessage
makeInfoMessage elements = LogMessage (Info) (toInt $ getElement elements 0) (buildMessage $ drop 1 elements)

makeWarningMessage :: [String] -> LogMessage
makeWarningMessage elements = LogMessage (Warning) (toInt $ getElement elements 0) (buildMessage $ drop 1 elements)

parseMessage :: String -> LogMessage
parseMessage raw
    | typeIndicator == "E" = makeErrorMessage $ drop 1 list
    | typeIndicator == "I" = makeInfoMessage $ drop 1 list
    | typeIndicator == "W" = makeWarningMessage $ drop 1 list
    where list = splitOn " " raw
          typeIndicator = getElement list 0


  
