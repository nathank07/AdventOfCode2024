import GHC.IO.Handle
import System.IO

readInput :: Handle -> IO [String]
readInput h = do
    eof <- hIsEOF h
    if eof then return [] else
        do
            line <- hGetLine h
            rest <- readInput h
            return $ line : rest


data Direction = Across | Below | BelowLeft | BelowRight

getDirection :: String -> [String] -> Direction -> String
getDirection searchStr belowStrs Across = take 3 $ drop 1 searchStr
getDirection searchStr belowStrs direction = getIdxs xs belowStrs
    where
        xs = case direction of
                Below -> replicate 3 strLoc
                BelowLeft -> [strLoc - x | x <- [1..3]]
                BelowRight -> [strLoc + x | x <- [1..3]]
        strLoc = if not (null belowStrs) then length (head belowStrs) - length searchStr else 0
        getIdxs (x:xs) (y:ys) = if x >= 0 && x < length y then (y !! x) : getIdxs xs ys else ""
        getIdxs _ _ = ""


getXmas :: [String] -> Int
getXmas ([]:xss) = getXmas xss
getXmas ((x:xs):xss) = case x of
    'S' -> getXmas (xs:xss) + hasCrosswordMatch "AMX"
    'X' -> getXmas (xs:xss) + hasCrosswordMatch "MAS"
    _ -> getXmas (xs:xss)
    where
        hasCrosswordMatch s = length $ filter id $ map (==s) [fn Across, fn Below, fn BelowLeft, fn BelowRight]
        fn = getDirection (x:xs) xss
getXmas _ = 0

getXXmas :: [String] -> Int
getXXmas ([]:xss) = getXXmas xss
getXXmas ((x:xs):xss) = getXXmas (xs:xss) + isXXMas
    where
        isXXMas = case (leftRight, rightLeft) of
            ("SAM", "SAM") -> 1
            ("MAS", "SAM") -> 1
            ("MAS", "MAS") -> 1
            ("SAM", "MAS") -> 1
            _ -> 0
        leftRight = x : take 2 (getDirection (x:xs) xss BelowRight)
        rightLeft = head (tail xs) : take 2 (getDirection (tail xs) xss BelowLeft)
getXXmas _ = 0

main :: IO()
main = do
    h <- openFile "input" ReadMode
    x <- readInput h
    print $ getXmas x
    print $ getXXmas x