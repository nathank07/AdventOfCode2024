import GHC.IO.Handle
import System.IO
import Data.List (transpose)

type Maze = [String]

readMap :: Handle -> IO Maze
readMap h = do
    eof <- hIsEOF h
    if eof then return [] else
        do
            line <- hGetLine h
            rest <- readMap h
            return (line : rest)

rotateGuard :: Char -> Char
rotateGuard c =
    case c of
        '^' -> '>'
        '>' -> 'v'
        'v' -> '<'
        '<' -> '^'
        c -> c

rotate :: Maze -> Maze
rotate maze = map (map rotateGuard . reverse) (transpose maze)

traverse :: Maze -> Maze
traverse m = go m [] 0
    where go :: Maze -> Maze -> Int -> Maze
          go (r:rs) acc rotated
            | done (concat (r:rs)) = iterate rotate (r:rs) !! (rotated `mod` 3)
            | '>' `notElem` concat (r:rs) = go (rotate (r:rs)) acc (rotated + 1)
            | '>' `notElem` r = go rs (acc ++ [r]) rotated
            | otherwise = go (acc ++ [walk r [] False] ++ rs) [] rotated

          walk (x:xs) acc False
            | x /= '>' = walk xs (acc ++ [x]) False
            | not (null xs) && head xs == '#' = acc ++ [rotateGuard x] ++ xs
            | otherwise = walk xs (acc ++ ['X']) True
            
          walk (x:xs) acc True = if x /= '#' then walk xs (acc ++ ">") True else acc ++ [rotateGuard x] ++ xs
          walk [] acc _ = acc
          done xs = not (any (\x -> x == '>' || x == '^' || x == '<' || x == 'v') xs)

main :: IO()
main = do
    h <- openFile "input" ReadMode
    m <- readMap h
    let a = Main.traverse m
    print $ length $ concatMap (filter (=='X')) a