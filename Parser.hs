import Text.ParserCombinators.ReadP
import Text.Printf

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
    
paddingZeros :: Int -> Int -> String
paddingZeros n p = printf ("%0" ++ (show n) ++ "d") p

main :: IO ()
main = putStr "Hello World"