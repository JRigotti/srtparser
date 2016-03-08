import Data.Time
import Data.Attoparsec.Char8

data LogEntry = 
    LogEntry { seq     :: Int
             , start   :: LocalTime
             , end     :: LocalTime
             , message :: String 
             } deriving Show

type Log = [LogEntry]

-- 00:02:20,476
parseTime :: String -> Parser LocalTime
parseTime s = do
    h <- count 2 digit
    char ':'
    m <- count 2 digit
    char ':'
    s <- count 2 digit
    char ','
    ms <- count 3 digit
    return $ Parser { localDay = fromGregorian 1 1 1
                    , localTimeOfDay = (read h) (read m) (read s) }

main :: IO ()
main = print $ parseOnly parseTime "00:02:20,476"