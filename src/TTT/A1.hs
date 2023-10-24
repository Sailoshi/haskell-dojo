module TTT.A1 where

import Data.Char (toUpper)

-- Q#01

_SIZE_ :: Int
_SIZE_ = 3

-- Q#02

_DISPLAY_LOGO_ :: Bool
_DISPLAY_LOGO_ = True

-- Q#03


convertRowIndex :: Char -> Int
convertRowIndex c = fromEnum (toUpper c) - 65


-- Q#04

_INVALID_MOVE_ = (-1, -1) 

-- Q#05

_SEP_ = "_|_"

-- Q#06

data Square = X | O | Neither deriving (Show, Eq)

-- Q#07

data GameState = X_Won | O_Won | Tie | Progress deriving (Show, Eq)    

-- Q#08

type Player = Square
type Row = [Square]
type Line = [Square]
type Board = [Row]
type Move = (Int, Int)

-- Q#09

getFirstPlayer :: Bool -> Player
getFirstPlayer i = if i then X else O

getFirstPlayer_ i | i = X
                  | otherwise = O

-- Q#10

showGameState :: GameState -> String
showGameState gs = case gs of
        X_Won -> "Player X won"
        O_Won -> "Player O won"
        Tie -> "Tie"
        Progress -> "Game in progress"
        

-- Q#11

switchPlayer :: Player -> Player
switchPlayer O = X
switchPlayer X = O
switchPlayer _ = Neither

-- Q#12

showSquare :: Square -> String
showSquare s | s == X = "X"
             | s == O = "O"
             | otherwise = "_"