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

addMult :: String -> MulState -> Int
addMult [] _ = 0
addMult [')'] (Num2 n1 n2) = n1 * n2
addMult [_] _ = 0
addMult (x1:xs) state = 
    case (state, x1) of
        (Invalid, 'm') -> if take 2 rest == "ul" then addMult (drop 2 rest) MulStart else addMult rest Invalid
        (Invalid, _) -> addMult rest Invalid
        (MulStart, '(') -> addMult rest LeftParen
        (LeftParen, _) -> checkThen $ addMult rest (Num1 $ digitToInt x1)
        (Num1 n, ',') -> addMult rest (Comma n)
        (Num1 n, _) -> checkThen $ addMult rest (Num1 ((n * 10) + digitToInt x1))
        (Comma n, _) -> checkThen $ addMult rest (Num2 n $ digitToInt x1)
        (Num2 n1 n2, ')') -> (n1 * n2) + addMult rest Invalid
        (Num2 n1 n2, _) -> checkThen $ addMult rest (Num2 n1 ((n2 * 10) + digitToInt x1))
        _ -> addMult rest Invalid 
        where checkThen fn = if not $ isNumber x1 then addMult rest Invalid else fn
              rest = xs


main :: IO()
main = do
    h <- openFile "input" ReadMode
    x <- readInput h
    print $ addMult x Invalid