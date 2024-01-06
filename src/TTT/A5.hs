module TTT.A5 where

import Control.Monad (when)
import System.Random.Stateful (globalStdGen, uniformM)
import TTT.A1
import TTT.A2
import TTT.A3
import TTT.A4

-- Q#01

printBoard :: Board -> IO ()
printBoard board = formatBoard board

-- Q#02
_LOGO_PATH_ :: FilePath
_LOGO_PATH_ = "./assets/ttt-logo.txt"

printLogo :: FilePath -> IO ()
printLogo filePath = readFile filePath >>= putStrLn

-- Q#03
_RANDOM_BOOL_ :: IO Bool
_RANDOM_BOOL_ = (uniformM globalStdGen:: IO Int) >>= (\randomNumber -> if even randomNumber then return True else return False)

firstPlayer :: IO Bool -> IO Player
firstPlayer booleanInput = booleanInput >>= (\i -> if i then return X else return O)

-- Q#04
getMove :: Board -> IO String
getMove board = getLine >>= newMove
    where
        newMove :: String -> IO String
        newMove move = if isValidMove board $ stringToMove move then return move else  putStrLn "Invalid Move, tryAgain" >> getMove board

-- Q#05

play :: Board -> Player -> IO ()
play board player = when _DISPLAY_LOGO_  (printLogo _LOGO_PATH_) >> printBoard board >> promptPlayer player >> getMove board >>= setMove >>= getGameState
    where
        setMove :: String -> IO (GameState, Board)
        setMove input = return (playMove player board (stringToMove input))
        getGameState :: (GameState, Board) -> IO ()
        getGameState (gs, board) = case gs of
            Tie -> printBoard board >> putStrLn "TIE"
            Progress -> play board (switchPlayer player)
            X_Won -> printBoard board >> putStrLn "Player X WON"
            O_Won -> printBoard board >> putStrLn "Player O WON"

-- Q#06

runTTT :: IO ()
runTTT = firstPlayer _RANDOM_BOOL_ >>= letsGo
    where
        letsGo :: Player -> IO ()
        letsGo player = play _EMPTY_BOARD_ (player:: Square)

-- Q#07

printLogoDo :: FilePath -> IO ()
printLogoDo filePath = do
    logo <- readFile filePath
    putStrLn logo

-- Q#08

firstPlayerDo :: IO Bool -> IO Player
firstPlayerDo booleanInput = do
    input <- booleanInput
    if input then return X else return O

-- Q#09

getMoveDo :: Board -> IO String
getMoveDo board = do
    let
        newMove :: String -> IO String
        newMove move = if isValidMove board $ stringToMove move then return move else  putStrLn "Invalid Move, tryAgain" >> getMove board
    move <- getLine        
    newMove move

-- Q#10

playDo :: Board -> Player -> IO ()
playDo board player = do
    let
        setMove :: String -> IO (GameState, Board)
        setMove input = return (playMove player board (stringToMove input))
        getGameState :: (GameState, Board) -> IO ()
        getGameState (gs, board) = case gs of
            Tie -> printBoard board >> putStrLn "TIE"
            Progress -> play board (switchPlayer player)
            X_Won -> printBoard board >> putStrLn "Player X WON"
            O_Won -> printBoard board >> putStrLn "Player O WON"
    when _DISPLAY_LOGO_  (printLogo _LOGO_PATH_)
    printBoard board
    promptPlayer player
    move <- getMove board
    gameState <- setMove move
    getGameState gameState
    
     
    
