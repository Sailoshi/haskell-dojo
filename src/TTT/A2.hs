module TTT.A2 where

import Data.List (intercalate)
import TTT.A1

-- Q#01

main = do
    putStrLn "Hello, what's your name?"
    name <- getLine
    putStrLn "Hey "

promptPlayer :: Player -> IO ()
promptPlayer p = do
    putStrLn startText
    --name <- getLine
    --putStrLn ("You've selected: " ++ name)
    where
        startText = concat ["Player",  show p, "'s turn: enter a row and column position (ex. A1): "]

-- Q#02

_RANGE_ = [0.._SIZE_ - 1]

-- Q#03

isDigit :: Char -> Bool
isDigit d = d `elem` ['0'..'9']

readDigit :: Char -> Int
readDigit d = if isDigit d then read [d] else -1

--readDigit d = if isDigit d then fromEnum d - 48 else -1

-- Q#04

_EMPTY_ROW_ :: [Square]
_EMPTY_ROW_ = replicate _SIZE_ Neither

_EMPTY_BOARD_ :: [[Square]]
_EMPTY_BOARD_ = replicate _SIZE_ _EMPTY_ROW_

-- Q#05

isTied :: Board -> Bool
isTied b = Neither `elem` concat b

_TIED_BOARD_ = [
        [X, O, O]
      , [O, Neither, X]
      , [O, X, O]
      ]

-- Q#06

indexRowStrings :: [String] -> [(Char, String)]
indexRowStrings a = zip b a
                    where
                     b  = ['A'..'Z']
-- Q#07

formatLine :: [String] -> String
formatLine a = _SEP_ ++ intercalate _SEP_  a ++ _SEP_ ++ "\n"

-- Q#08

isMoveInBounds :: Move -> Bool
isMoveInBounds (r, c) | r >= 0 && r < _SIZE_ && c >= 0 && c < _SIZE_ = True
                      | otherwise = False

-- Q#09

stringToMove :: String -> Move
stringToMove [a, b] = (convertRowIndex a, readDigit b)
stringToMove _ = _INVALID_MOVE_

-- Q#10

replaceSquareInRow :: Player -> Int -> Row -> Row
replaceSquareInRow _ _ [] = []
replaceSquareInRow p c r = if c < 0 || c >= _SIZE_ then r else xs ++ [p] ++ ys
                    where
                        (xs, (x:ys)) = splitAt c r

rsX = replaceSquareInRow X
rsO = replaceSquareInRow O
rsN = replaceSquareInRow Neither
