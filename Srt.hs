module Srt (
    SrtTime (SrtTime), 
    LogEntry (LogEntry),
    Op (..),
    SyncOp,
    syncLog
) where

import Data.List (intercalate)
import Text.Printf (printf)

type SyncOp = (Op, Int)

data Op = Delay | Forward deriving Show

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
    show (LogEntry logn st end msg) = (intercalate "\n" [(show logn), (show st) ++ " --> " ++ (show end), msg]) ++ "\n"
    
syncLog :: LogEntry -> SyncOp -> LogEntry
syncLog (LogEntry ln st ed msg) op = LogEntry ln (syncTime st op) (syncTime ed op) msg
    
syncTime :: SrtTime -> SyncOp -> SrtTime
syncTime st op = milliToSrtTime $ (srtTimeToMilli st) `fn` t
  where t  = snd op
        fn = case fst op of Delay   -> (-)
                            Forward -> (+)
    
paddingZeros :: Int -> Int -> String
paddingZeros n p = printf ("%0" ++ (show n) ++ "d") p

milliToSrtTime :: Int -> SrtTime
milliToSrtTime t 
  | t >= 86400000 = error "The number for conversion is too high"
  | otherwise = SrtTime h m s ms
  where  ms = t `mod` 1000
         s  = (t `div` 1000) `mod` 60
         m  = (t `div` 60000) `mod` 60
         h  = t `div` 3600000

srtTimeToMilli :: SrtTime -> Int
srtTimeToMilli st =
  let h  = (hours st) * 3600000
      m  = (minutes st) * 60000
      s  = (seconds st) * 1000
      ms = milli st
  in h + m + s + ms 