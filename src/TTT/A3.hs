module TTT.A3 where

import Data.List (transpose)

import TTT.A1
import TTT.A2

-- Q#01

showInts ::  [Int] -> [String]
showInts (x:xs) = show x : showInts xs
showInts [] = []

_HEADER_ :: String
_HEADER_ =  formatLine $ " " : showInts [0, 1, 2 , 3]

-- Q#02

showSquares :: [Square] -> [String]
showSquares (x:xs) = if x == Neither then "" : showSquares xs else show x : showSquares xs
showSquares [] = []

-- Q#03

formatRows ::  [Row] ->  [String]
formatRows (x : xs) =  formatLine (showSquares x) : formatRows xs
formatRows [] = []

-- Q#04

isColEmpty :: Row -> Int -> Bool
isColEmpty [] _ = False
isColEmpty x i = x !! i == Neither

-- Q#05

dropFirstCol :: Board -> Board
dropFirstCol (xs:xss) = [tail xs] ++ dropFirstCol xss
dropFirstCol [] = []

dropLastCol :: Board -> Board
dropLastCol (xs:xss) = [init xs] ++ dropLastCol xss
dropLastCol [] = []

-- Q#06

getDiag1 :: Board -> Line
getDiag1 (x:xs) = head x : (getDiag1 (dropFirstCol xs))
getDiag1 [] = []

getDiag2 :: Board -> Line
getDiag2 (x:xs) = last x : (getDiag2 (dropLastCol xs))
getDiag2 [] = []

getAllLines :: Board -> [Line]
getAllLines b =  (b ++ transpose b ++ [getDiag1 b] ++ [getDiag2 b])

-- Q#07
putSquare :: Player -> Board -> Move -> Board
putSquare p b (rowIndex, cIndex) = go rowIndex 0 b
    where
        go :: Int -> Int -> Board -> Board
        go selectedRowIndex currentRowIndex (xs: (y:ys)) = if selectedRowIndex == currentRowIndex then replaceSquareInRow p cIndex y : (y:ys) else y : go selectedRowIndex (currentRowIndex+1) (y:ys)
        go _ _ [ys:yss] = [replaceSquareInRow p cIndex (ys:yss)]
        go _ _ _ = []

-- Q#08

prependRowIndices :: [String] -> [String]
prependRowIndices input = go $ indexRowStrings input
    where
        go :: [(Char, String)] -> [String]
        go ((c,s):xs) = (c : s::[Char] ) : go xs
        go _ = []

-- Q#09

isWinningLine :: Player -> Line -> Bool
isWinningLine p line= go p line False
    where
        go :: Player -> Line -> Bool -> Bool
        go player (x:xs) isWinningLine = player == x && go player xs (player == x)
        go _  [] isWinningLine = isWinningLine

-- Q#10

isValidMove :: Board -> Move -> Bool
isValidMove board (rowIndex, colIndex) = go 0 board (rowIndex, colIndex) $ (isMoveInBounds (rowIndex, colIndex) && not ( null board))
    where
        go :: Int -> Board -> Move -> Bool -> Bool
        go currentRowIndex (xs:xss) (rowIndex, colIndex) isMoveInBounds = if rowIndex == currentRowIndex then isColEmpty xs colIndex else go (currentRowIndex + 1) xss (rowIndex, colIndex) isMoveInBounds
        go _ _ _ isMoveInBounds = isMoveInBounds