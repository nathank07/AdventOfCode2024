import GHC.IO.Handle
import System.IO


-- https://stackoverflow.com/a/7569301
splitBy :: (Foldable t, Eq a) => a -> t a -> [[a]]
splitBy delimiter = foldr f [[]]
            where f c l@(x:xs) | c == delimiter = []:l
                               | otherwise = (c:x):xs

type Rules = [(Int, Int)]
type Order = [Int]

readPageRules :: Handle -> IO Rules
readPageRules beginHandle = do
    line <- hGetLine beginHandle
    if line == "" then return [] else
        do
            let first = rule line
            rest <- readPageRules beginHandle
            return (first : rest)
            where
                rule :: String -> (Int, Int)
                rule l = (read $ takeWhile (/= '|') l, (read . tail) $ dropWhile (/= '|') l)

readPageOrders :: Handle -> IO [Order]
readPageOrders endHandle = do
    eof <- hIsEOF endHandle
    if eof then return [] else
        do
            line <- hGetLine endHandle
            rest <- readPageOrders endHandle
            let nums = map (read :: String -> Int) $ splitBy ',' line
            return (nums : rest)

intersect :: Order -> Order -> Order
intersect [] _ = []
intersect _ [] = []
intersect xs ys = filter (`elem` xs) ys

checkPageOrder :: Rules -> Order -> Bool
checkPageOrder r [] = True
checkPageOrder r (x:xs) = isBehindEach && checkPageOrder r xs
    where isBehindEach = null $ intersect xs (map fst $ filter (\(a, b) -> b == x) r)

main :: IO()
main = do
    h <- openFile "input" ReadMode
    rules <- readPageRules h
    orders <- readPageOrders h
    print $ sum $ map (\xs -> xs !! (length xs `div` 2)) $ filter (checkPageOrder rules) orders