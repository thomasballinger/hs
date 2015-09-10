import Control.Exception
import Control.Concurrent
import Control.Monad
import System.IO

import TetrisGame
import Terminal
import Music


blockDisplay n = ("   " : [colored color "xxx" | color <- ["red", "yellow", "green"]])!!n

display game = boardDisplay (boardView game)

boardDisplay board = do
    putStrLn ("\n" ++ "+" ++ replicate 30 '-' ++ "+")
    displayLines board

displayLines lines =
    if null lines
        then do
            putStr $ "+" ++ replicate 30 '-' ++ "+"
            hFlush stdout
        else
            let line = head lines in
                do
                    putStrLn $ "|" ++ concat [blockDisplay x | x <- line] ++ "|"
                    putStrLn $ "|" ++ concat [blockDisplay x | x <- line] ++ "|"
                    displayLines (tail lines)

tick :: Game -> Int -> IO Game
tick g i = do
    clear
    display g
    c <- getChar
    let newgame = gameTick g c
    return newgame

mainIO = foldM_ tick newGame [0..]

main = do
    putStrLn $ colored "red" "Let's play Tetris!"
    playMusic
    bracket_
        (do
            hSetBuffering stdin NoBuffering
            hSetEcho stdin False)
        (do
            hSetBuffering stdin LineBuffering
            hSetEcho stdin True
            putStrLn "bye"
            )
        mainIO

