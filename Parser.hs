import Text.ParserCombinators.ReadP
import Text.Printf
import Data.List (intercalate)
import Control.Applicative

-- Creating an specific dateformat
-- used in srt files
data SrtTime = SrtTime { hours   :: Int
                       , minutes :: Int
                       , seconds :: Int
                       , milli   :: Int }

instance Show SrtTime where
    show (SrtTime h m s ms) = 
        let pz = paddingZeros
        in (pz 2 h) ++ ":" ++ (pz 2 m) ++ ":" ++ (pz 2 s) ++ "," ++ (pz 3 ms)

data LogEntry = LogEntry { logNumber :: Int
                         , start     :: SrtTime
                         , end       :: SrtTime
                         , message   :: String }
                         
instance Show LogEntry where
    show (LogEntry logn st end msg) = intercalate "\n" [(show logn), (show st) ++ " --> " ++ (show end), msg]
    
paddingZeros :: Int -> Int -> String
paddingZeros n p = printf ("%0" ++ (show n) ++ "d") p

isDigit :: Char -> Bool
isDigit c = any (c==) ['0'..'9']

digit :: ReadP Char
digit = satisfy isDigit

digits :: ReadP Int
digits = do
  parse <- many1 digit
  return $ read parse
  
srttime :: ReadP SrtTime
srttime = do
  h <- count 2 digit
  char ':'
  m <- count 2 digit
  char ':'
  s <- count 2 digit
  char ','
  ms <- count 3 digit
  return $ SrtTime (read h) (read m) (read s) (read ms)
  

main :: IO ()
main = do
  readFile "example.srt" >>= putStrLn