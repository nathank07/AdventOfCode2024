import GHC.IO.Handle
import System.IO
import Data.List
import Data.Char

readInput :: Handle -> IO String
readInput h = do
    eof <- hIsEOF h
    if eof then return "" else
        do
            line <- hGetLine h
            rest <- readInput h
            return $ line ++ rest

data MulState = Invalid | MulStart | LeftParen | Num1 Int | Comma Int | Num2 Int Int

data DoState = Do | Dont 
    deriving (Eq)

addMult :: String -> MulState -> DoState -> Int
addMult [] _ _ = 0
addMult [')'] (Num2 n1 n2) Do = n1 * n2
addMult [_] _  _ = 0
addMult (x1:xs) state dostate
    | take 4 (x1:xs) == "do()" = addMult xs Invalid Do
    | take 7 (x1:xs) == "don't()" = addMult xs Invalid Dont
    | otherwise = 
    case (state, x1) of
        (Invalid, 'm') -> if take 2 rest == "ul" then addMult (drop 2 rest) MulStart dostate else addMult rest Invalid dostate
        (Invalid, _) -> addMult rest Invalid dostate
        (MulStart, '(') -> addMult rest LeftParen dostate
        (LeftParen, _) -> checkThen $ addMult rest (Num1 $ digitToInt x1) dostate
        (Num1 n, ',') -> addMult rest (Comma n) dostate
        (Num1 n, _) -> checkThen $ addMult rest (Num1 ((n * 10) + digitToInt x1)) dostate
        (Comma n, _) -> checkThen $ addMult rest (Num2 n $ digitToInt x1) dostate
        (Num2 n1 n2, ')') -> (n1 * n2 * if dostate == Do then 1 else 0) + addMult rest Invalid dostate
        (Num2 n1 n2, _) -> checkThen $ addMult rest (Num2 n1 ((n2 * 10) + digitToInt x1)) dostate
        _ -> addMult rest Invalid dostate 
        where checkThen fn = if not $ isNumber x1 then addMult rest Invalid dostate else fn
              rest = xs

main :: IO()
main = do
    h <- openFile "input" ReadMode
    x <- readInput h
    print $ addMult x Invalid Do