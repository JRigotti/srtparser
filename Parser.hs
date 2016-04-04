module Parser where

import Text.ParserCombinators.ReadP
import Control.Applicative ((<|>))
import Data.Char (isAscii, isDigit)
import Srt
import System.Environment (getArgs)
import System.FilePath (replaceFileName, takeFileName)

digits :: ReadP Int
digits = fmap read $ munch1 isDigit
  
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

eol :: ReadP ()
eol = (string "\n" <|> string "\r" <|> string "\n\r" <|> string "\r\n") >> return ()

srtmsg :: ReadP String
srtmsg = do
  l <- manyTill (satisfy isAscii) ((count 2 eol) <++ (count 1 eof))
  return l
      
srtentry :: ReadP LogEntry
srtentry = do
  num <- digits
  eol
  st  <- srttime
  string " --> "
  end <- srttime
  eol
  msg <- srtmsg
  return $ LogEntry num st end msg
  
parseToSrtFormat :: ReadP [LogEntry] -> String -> SyncOp -> String
parseToSrtFormat p s op = case readP_to_S p s of
  [] -> error "Error parsing the file"
  ps -> (unlines . map (show . (flip syncLog op)) . fst . last) ps
  
operation :: String -> SyncOp
operation ('+':xs) = (Forward, read xs)
operation ('-':xs) = (Delay, read xs)
operation xs       = (Forward, read xs)
  
main :: IO ()
main = do
  (filepath:optime:args) <- getArgs -- args passed by the user
  let op = operation optime
  contents <- readFile filepath
  let outputFile = replaceFileName filepath ("new_" ++ takeFileName filepath)
      parsedSrt = parseToSrtFormat (many1 srtentry) contents op
  writeFile outputFile parsedSrt
  putStrLn $ "New file created: " ++ outputFile
  