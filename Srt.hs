module Srt (
    SrtTime (SrtTime), 
    LogEntry (LogEntry)
) where

import Data.List (intercalate)
import Text.Printf (printf)

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
    
paddingZeros :: Int -> Int -> String
paddingZeros n p = printf ("%0" ++ (show n) ++ "d") p