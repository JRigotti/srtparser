module Parser where

import Text.ParserCombinators.ReadP
import Control.Applicative
import Data.Char (isAscii, isDigit)
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
  
srtentry :: ReadP LogEntry
srtentry = do
  num <- digits
  eol
  st  <- srttime
  string " --> "
  end <- srttime
  eol
  msg <- manyTill (satisfy isAscii) (count 2 eol)
  return $ LogEntry num st end msg
