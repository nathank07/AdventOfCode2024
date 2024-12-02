
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

-- What a gross solution :/

data IsSafe = AscSafe | AscUnsafe | DescSafe | DescUnsafe | None | NoneUnsafe

numsSafe :: Int -> Int -> IsSafe -> Bool
numsSafe x1 x2 safety 
    | x1 == x2 = False
    | otherwise = case safety of
                    AscSafe    -> x2 - x1 <= 3 && x2 - x1 > 0
                    DescSafe   -> x1 - x2 <= 3 && x1 - x2 > 0
                    AscUnsafe  -> numsSafe x1 x2 AscSafe
                    DescUnsafe -> numsSafe x1 x2 DescSafe
                    None       -> numsSafe x1 x2 AscSafe || numsSafe x1 x2 DescSafe
                    NoneUnsafe -> numsSafe x1 x2 None

safeReport :: [Int] -> IsSafe -> Bool
safeReport []  _ = True
safeReport [x] _ = True
safeReport (x1:x2:xs) safety =
    case safety of
        NoneUnsafe -> safe && safeReport (x2:xs) (if x1 < x2 then AscUnsafe else DescUnsafe)
        AscUnsafe  -> safe && safeReport (x2:xs) AscUnsafe
        DescUnsafe -> safe && safeReport (x2:xs) DescUnsafe
        None       -> safe && safeReport (x2:xs) (if x1 < x2 then AscSafe else DescSafe) || safeReport (x1:xs) NoneUnsafe || safeReport (x2:xs) NoneUnsafe
        AscSafe    -> safe && safeReport (x2:xs) AscSafe  || safeReport (x1:xs) AscUnsafe
        DescSafe   -> safe && safeReport (x2:xs) DescSafe || safeReport (x1:xs) DescUnsafe
        where safe = numsSafe x1 x2 safety


safeReports :: [[Int]] -> Bool -> Int
safeReports xs dampen = length $ filter id $ map (`safeReport` if dampen then None else NoneUnsafe) xs

main :: IO ()
main = do
    h <- openFile "input" ReadMode
    x <- readInput h
    print $ safeReports x False
    print $ safeReports x True

