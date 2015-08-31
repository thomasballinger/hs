import Control.Concurrent
import Control.Exception
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

tick g = do
    clear
    display g
    threadDelay 50000

clear = do
    putStr "\x1b[2J"
    hFlush stdout

cleanup = do
    hSetBuffering stdin LineBuffering
    hSetEcho stdin True
    print ""

mainIO = do
    putStrLn $ colored "red" "Let's play Tetris!"
    let games = take 60 gamestates
    rs <- sequence (map tick games)
    print rs

main = bracket_
    (do
        hSetBuffering stdin NoBuffering
        hSetEcho stdin False)
    cleanup
    mainIO

