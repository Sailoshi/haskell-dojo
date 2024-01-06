module HM.A6 where

import Data.Char (isAlpha)
import HM.Provided

-- Q#01

type Chances = Int
type Guess = String
type Move = Char
type Secret = String
type Dictionary = [String]

-- Q#02
data GameException = InvalidChars | InvalidLength | NotInDict | InvalidMove | RepeatMove | GameOver | WonGame

-- Q#03

lengthInRange :: Secret -> Bool
lengthInRange secret = secretLength >= fst _LENGTH_ && secretLength <= snd _LENGTH_
    where
        secretLength = length secret

-- Q#04

invalidMove :: Move -> Bool
invalidMove input = not $ isAlpha input

-- Q#05

revealLetters :: Move -> Secret -> Guess -> Guess
revealLetters move secret guess = zipWith (\a b -> if a == move then a else b) secret guess

-- Q#06

updateChances :: Move -> Secret -> Chances -> Chances
updateChances move secret chances = if chances > 0 && notElem move secret then chances - 1 else chances

-- Q#07

setSecret :: IO Secret
setSecret = do
    _ <- putStr "Enter a secret word:\t"
    _ <- showInput False
    secret <- getLine
    _ <- showInput True
    _SPACE_
    putStrLn $ "***" ++ secret ++ "***"
    pure secret