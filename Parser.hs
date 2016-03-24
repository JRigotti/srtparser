module Parser where

import Text.ParserCombinators.ReadP
import Control.Applicative ((<|>))
import Data.Char (isPrint, isDigit)
import Srt

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

line :: ReadP String
line = do
  l <- munch1 isPrint
  skipSpaces
  return l
  
srtentry :: ReadP LogEntry
srtentry = do
  num <- digits
  eol
  st  <- srttime
  string " --> "
  end <- srttime
  eol
  msg <- many1 line
  return $ LogEntry num st end msg
  
main :: IO ()
main = do
  contents <- readFile "example.srt"
  print $ readP_to_S (many1 srtentry) contents