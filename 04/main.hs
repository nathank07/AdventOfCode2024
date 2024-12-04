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

getBelow :: String -> [String] -> String
getBelow searchStr belowStrs = getIdxs (replicate 3 strLoc) belowStrs
    where getIdxs (x:xs) (y:ys) = if x >= 0 && x < length y then (y !! x) : getIdxs xs ys else ""
          getIdxs _ _ = ""
          strLoc = if length belowStrs > 0 then length (head belowStrs) - length searchStr else 0

getBottomLeft :: String -> [String] -> String
getBottomLeft searchStr belowStrs = getIdxs idxs belowStrs
    where idxs = [strLoc - x | x <- [1..3]]
          getIdxs (x:xs) (y:ys) = if x >= 0 && x < length y then (y !! x) : getIdxs xs ys else ""
          getIdxs _ _ = ""
          strLoc = if length belowStrs > 0 then length (head belowStrs) - length searchStr else 0

getBottomRight :: String -> [String] -> String
getBottomRight searchStr belowStrs = getIdxs idxs belowStrs
    where idxs = [strLoc + x | x <- [1..3]]
          getIdxs (x:xs) (y:ys) = if x >= 0 && x < length y then (y !! x) : getIdxs xs ys else ""
          getIdxs _ _ = ""
          strLoc = if length belowStrs > 0 then length (head belowStrs) - length searchStr else 0

getXmas :: [String] -> Int
getXmas ([]:xss) = getXmas xss
getXmas [x:xs] = case x of
    'S' -> getXmas [xs] + if take 3 xs == "AMX" then 1 else 0
    'X' -> getXmas [xs] + if take 3 xs == "MAS" then 1 else 0
    _ -> getXmas [xs]
getXmas ((x:xs):xss) = case x of
    'S' -> getXmas (xs:xss) + across "AMX" + below "AMX" + bottomL "AMX" + bottomR "AMX"
    'X' -> getXmas (xs:xss) + across "MAS" + below "MAS" + bottomL "MAS" + bottomR "MAS"
    _ -> getXmas (xs:xss)
    where below str = if getBelow (x:xs) xss == str then 1 else 0
          across str = if take 3 xs == str then 1 else 0
          bottomL str = if getBottomLeft (x:xs) xss == str then 1 else 0
          bottomR str = if getBottomRight (x:xs) xss == str then 1 else 0
getXmas _ = 0


main :: IO()
main = do
    h <- openFile "input" ReadMode
    x <- readInput h
    print $ getXmas x