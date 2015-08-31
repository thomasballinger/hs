import Control.Concurrent
import Control.Exception
import Control.Monad
import System.IO

import TetrisGame
import Terminal


blockDisplay n = (" " : [colored color "x" | color <- ["red", "yellow"]])!!n

display game = boardDisplay (boardView game)

boardDisplay board = do
    putStrLn ("\n" ++ replicate 10 '-')
    displayLines board

displayLines lines =
    if null lines
        then do
            putStr (replicate 10 '-')
            hFlush stdout
        else
            let line = head lines in
                do
                    putStrLn (concat [blockDisplay x | x <- line])
                    displayLines (tail lines)

tick :: Game -> Int -> IO Game
tick g i = do
    clear
    display g
    c <- getChar
    let newgame = gameTick g c
    return newgame

clear = do
    putStr "\x1b[2J"
    hFlush stdout

cleanup = do
    hSetBuffering stdin LineBuffering
    hSetEcho stdin True
    print ""

mainIO = do
    putStrLn $ colored "red" "Let's play Tetris!"
    foldM_ tick newGame [0..]

main = bracket_
    (do
        hSetBuffering stdin NoBuffering
        hSetEcho stdin False)
    cleanup
    mainIO

