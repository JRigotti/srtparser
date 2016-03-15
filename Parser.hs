module Parser where

import Text.ParserCombinators.ReadP
import Control.Applicative
import Data.Char (isPrint, isDigit)
import Srt


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
