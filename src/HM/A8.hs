module HM.A8 where

import Control.Monad (when)
import Data.Char (toUpper)
import HM.A6
import HM.A7 hiding (validateNoDict, validateWithDict)
import HM.Provided
import System.Directory (doesFileExist)

-- Q#01

getUpperChar :: IO Char
getUpperChar = fmap toUpper getChar

-- Q#02

_DICT_ :: IO Dictionary
_DICT_ = do
  fileExists <- doesFileExist _DICT_FILE_
  if fileExists then lines <$> readFile _DICT_FILE_ else pure []

isDictNonEmpty :: IO Bool
isDictNonEmpty =  not . null <$> _DICT_

-- Q#03

makeGameIfValid :: Either GameException Secret -> Either GameException Game
makeGameIfValid a  = case a  of
  Left a -> Left a
  Right b -> Right $ makeGame b

-- Q#04

getDict :: IO (Maybe Dictionary)
getDict = let mayBeDict = toMaybe <$> isDictNonEmpty
          in
              mayBeDict <*> _DICT_

-- Q#05

validateNoDict :: Secret -> Either GameException Secret
validateNoDict s = hasValidChars s >>= (\validChars -> if not validChars then Left InvalidChars else isValidLength s >>= (\validLength -> if validLength then Right s else Left InvalidLength))

validateWithDict :: Dictionary -> Secret -> Either GameException Secret
validateWithDict d s = isInDict d s >>= (\isExisting -> if isExisting then Right s else Left NotInDict)

-- Q#06

playGame :: Game -> IO ()
playGame g = do
   promptGuess
   upperChar <- getUpperChar
   _SPACE_
   case processTurn upperChar g of
    Left GameOver -> print GameOver >> pure ()
    Left WonGame -> putStrLn (showGameHelper (guess $ updateGame (toUpper upperChar) g) (moves $ updateGame (toUpper upperChar) g) (chances $ updateGame (toUpper upperChar) g)) >> print WonGame >> pure ()
    Right updatedGame -> putStrLn (showGameHelper (guess updatedGame) (moves updatedGame) (chances updatedGame)) >> playGame updatedGame
    Left RepeatMove -> print RepeatMove >> playGame g
    Left a -> print a >> playGame g
   return ()

-- Q#07

startGame :: (Secret -> Either GameException Secret) -> IO ()
startGame validateSecret = do
    secret <- setSecret
    case validateSecret secret of
        Left g -> print g >> startGame validateSecret
        Right s -> print (makeGame s) >> playGame (makeGame s)

-- Q#08

runHM :: IO ()
runHM = do
  maybeDict <- getDict

  case maybeDict of
    Nothing -> print "Missing dictionary! Continue without dictionary? [Y/N]" >> getUpperChar >>= (\input -> if input == 'N' then pure ()
      else startGame validateNoDict
        )
    Just a -> startGame (validateWithDict a)