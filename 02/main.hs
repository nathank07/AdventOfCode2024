
import GHC.IO.Handle
import System.IO
import Data.List

readInput :: Handle -> IO [[Int]]
readInput h = do
    eof <- hIsEOF h
    if eof then return [] else
        do
            line <- hGetLine h
            let nums = map read $ words line :: [Int]
            rest <- readInput h
            return $ nums : rest

data IsSafe = Asc | Desc | None

safeReport :: [Int] -> IsSafe -> Bool
safeReport []  _ = True
safeReport [x] _ = True
safeReport (x1:x2:xs) safety
    | x1 == x2 = False
    | otherwise = case safety of
        None -> safeReport ([x1, x2] ++ xs) (if x1 < x2 then Asc else Desc)
        Asc  -> x2 - x1 <= 3 && x2 - x1 > 0 && safeReport (x2:xs) Asc
        Desc -> x1 - x2 <= 3 && x1 - x2 > 0 && safeReport (x2:xs) Desc

safeReports :: [[Int]] -> Int
safeReports xs = length $ filter id $ map (`safeReport` None) xs

main :: IO ()
main = do
    h <- openFile "input" ReadMode
    x <- readInput h
    print $ safeReports x