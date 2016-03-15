import Text.ParserCombinators.ReadP
import Text.Printf
import Data.List (intercalate)
import Control.Applicative
import Data.Char (isPrint, isDigit)

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

digits :: ReadP Int
digits = do
  parse <- many1 (satisfy isDigit)
  return $ read parse
  
srttime :: ReadP SrtTime
srttime = do
  h <- digits
  char ':'
  m <- digits
  char ':'
  s <- digits
  char ','
  ms <- digits
  return $ SrtTime h m s ms

eol :: ReadP String
eol = string "\n"
  <|> string "\r"
  <|> string "\n\r"
  <|> string "\r\n"  
  
srtentry :: ReadP LogEntry
srtentry = do
  num <- digits
  eol
  st  <- srttime
  string " --> "
  end <- srttime
  eol
  msg <- manyTill (satisfy isPrint) (count 2 eol)
  return $ LogEntry num st end msg
