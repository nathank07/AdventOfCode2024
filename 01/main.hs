
import GHC.IO.Handle
import System.IO
import Data.List

readInput :: Handle -> IO ([Int], [Int])
readInput h = do
    eof <- hIsEOF h
    if eof then return ([], []) else 
        do
            line <- hGetLine h
            let l = (read . head . words $ line) :: Int
            let r = (read . last . words $ line) :: Int
            rest <- readInput h
            return (l : fst rest, r : snd rest)

distance :: ([Int], [Int]) -> Int
distance (l, r) = foldr (\x acc -> abs (fst x - snd x) + acc) 0 $ zip (sort l) (sort r)

similarity :: ([Int], [Int]) -> Int
similarity (l, r) = foldr (\x acc -> acc + (count (fst x) r * fst x)) 0 $ zip l r 
    where count x = length . filter (==x)

main :: IO ()
main = do
    h <- openFile "input" ReadMode
    x <- readInput h
    print $ distance x
    print $ similarity x
