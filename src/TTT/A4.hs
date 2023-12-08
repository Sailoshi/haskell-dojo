module TTT.A4 where

import Data.List (transpose)
import TTT.A1
import TTT.A2
import TTT.A3 (getAllLines, putSquare)

-- Q#01

_HEADER_ :: String
_HEADER_ = formatLine $ "'_'" : map show _RANGE_

-- Q#02

showSquares :: [Square] -> [String]
showSquares squares = map (\x -> if x == Neither then "E" else show x) squares

-- Q#03

dropFirstCol :: Board -> Board
dropFirstCol board = map tail board

-- Q#04

dropLastCol :: Board -> Board
dropLastCol board = map init board

--Q#05

--formatRows ::  [Row] ->  [String]
--formatRows (x : xs) =  formatLine (showSquares x) : formatRows xs
--formatRows [] = []

formatRows :: [Row] -> [String]
formatRows rows = map (\row -> formatLine $ showSquares row) rows

-- Q#06

--isWinningLine :: Player -> Line -> Bool
--isWinningLine p line= go p line False
--    where
--        go :: Player -> Line -> Bool -> Bool
--        go player (x:xs) isWinningLine = player == x && go player xs (player == x)
--        go _  [] isWinningLine = isWinningLine

isWinningLine :: Player -> Line -> Bool
isWinningLine selectedPlayerSquare line = length (filter (== selectedPlayerSquare) line) == 3

-- Q#07

isWinningLine_ :: Player -> Line -> Bool
isWinningLine_ selectedPlayerSquare line = foldr (\playerSquare acc -> if playerSquare == selectedPlayerSquare && acc then True else False) (not $ null line) line

-- Q#08

x = [ [X, O, O]
            , [O, X, O]
            , [O, O, X]
            ]

o = [ [O, X, O]
            , [X, X, O]
            , [X, O, O]
            ]

t = _TIED_BOARD_

tie = [ [O, X, O]
            , [X, X, O]
            , [X, O, X]
            ]


hasWon :: Player -> Board -> Bool
hasWon player board = foldr (\row acc -> isWinningLine player row || acc) False (getAllLines board)

-- Q#09

getGameState :: Board -> GameState
getGameState board  |  hasWon X board = X_Won
                    |  hasWon O board = O_Won
                    |  not (null (filter (\line -> Neither `elem` line) (getAllLines board))) = Progress
                    |  otherwise = Tie

playMove :: Player -> Board -> Move -> (GameState, Board)
playMove player board move =
                     (getGameState newBoard, newBoard)
                where
                    newBoard = putSquare player board move


prependRowIndices :: [String] -> [String]
prependRowIndices input = zipWith (\input letter -> letter ++ ". " ++ input) input (map (: []) ['A' .. 'Z'] )

-- Q#11

formatBoard :: Board -> IO ()
formatBoard board = putStr _HEADER_ >> putStr ( formatLine (prependRowIndices $ formatRows board) )
