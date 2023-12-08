module HM.A7 where

import Data.Char (isAlpha, toLower, toUpper)
import HM.A6
import HM.Provided
import System.Directory (doesFileExist)
import Data.List (intersperse, sort)
import GHC.IO.Exception (IOErrorType(InvalidArgument))
import GHC.IO.Encoding (CodingProgress(InvalidSequence))

-- Q#01
data Game = Game {
                    secret:: String,
                    guess:: String,
                    moves:: [Move],
                    chances:: Int
                 } deriving Show

-- Q#02

repeatedMove:: Move -> Game -> Bool
repeatedMove move game = move `elem` moves game

-- Q#03

_TEST_GAME_ =  Game
                      {
                        secret = "KAFFEEKANNE",
                        guess = "___________",
                        moves = [],
                        chances = _CHANCES_
                      }

makeGame :: Secret -> Game
makeGame secret = Game
                      {
                        secret = map toUpper secret,
                        guess = map (const '_') secret,
                        moves = [],
                        chances = _CHANCES_
                      }

-- Q#04 TODO: Improve by using the input game object.

updateGame :: Move -> Game -> Game
updateGame move game =
    do
      Game
                      {
                        secret = secret game,
                        guess = changedGuess,
                        moves = moves game ++ [move],
                        chances = updateChances move (secret game) (chances game)
                      }
    where
      changedGuess :: Guess
      changedGuess = revealLetters move (secret game) (guess game)



-- Q#05

showGameHelper :: String -> [Char] -> Int -> String
showGameHelper game moves chances =
  unlines
    [ _STARS_,
      "\tSecret Word:\t" ++ intersperse ' ' game ++ "\n",
      "\tGuessed:\t" ++ intersperse ' ' (sort moves) ++ "\n",
      "\tChances:\t" ++ show chances,
      _STARS_
    ]

-- Q#06
instance Show GameException where
  show :: GameException -> String
  show InvalidChars = "Invalid char"
  show InvalidLength = "Invalid length (" ++ lb ++ ", " ++ ub ++ ")"
                        where
                          lb = show $ fst _LENGTH_
                          ub = show $ snd _LENGTH_
  show NotInDict = "Not in dict"
  show InvalidMove = "Wrong move"
  show RepeatMove = "Please repeat your move"
  show GameOver = "Game is over"





-- Q#07

toMaybe :: Bool -> a -> Maybe a
toMaybe False a = Nothing
toMaybe True a = Just a

-- Q#08

validateSecret :: (Secret -> Bool) -> GameException -> Secret -> Either GameException Bool
validateSecret customValidation g s = if customValidation s then Right True else Left g

-- Q#09

hasValidChars :: Secret -> Either GameException Bool
hasValidChars s = validateSecret hasLetters InvalidChars s
                  where
                    hasLetters:: Secret -> Bool
                    hasLetters s = any (\l -> not $ isAlpha l) s

isValidLength :: Secret -> Either GameException Bool
isValidLength s = validateSecret lengthInRange InvalidLength s

isInDict :: Dictionary -> Secret -> Either GameException Bool
isInDict d s = validateSecret isInDictPred NotInDict s
                where
                  isInDictPred:: Secret -> Bool
                  isInDictPred s = any (\w -> map toLower s == map toLower w) d

-- Q#10

validateNoDict :: Secret -> Either GameException Secret
validateNoDict s = case hasValidChars s of
  Left g -> Left g
  Right _ -> case isValidLength s of
      Left g-> Left g
      Right _ -> Right s

validateWithDict :: Dictionary -> Secret -> Either GameException Secret
validateWithDict d s = case isInDict d s of
  Left g -> Left g
  Right _ -> Right s

-- Q#11

processTurn :: Move -> Game -> Either GameException Game
processTurn m g
      | invalidMove m = Left InvalidMove
      | repeatedMove m g = Left RepeatMove
      | chances g == 0 = Left GameOver
      | otherwise = Right updatedGame
      where
        updatedGame = updateGame  (toUpper m) g