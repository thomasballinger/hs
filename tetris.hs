import Control.Concurrent
import System.IO

import TetrisGame
import Terminal


blockDisplay n = ([" "] ++ [colored color "x" | color <- ["red", "yellow"]])!!n

display game = boardDisplay (boardView game)

boardDisplay board = do
    putStrLn (take 10 (repeat '-'))
    displayLines board

displayLines lines = do
    if null lines
        then putStrLn (take 10 (repeat '-'))
        else
            let line = head lines in
                do
                    putStrLn (concat [if x == 0 then " " else colored "red" "x" | x <- line])
                    displayLines (tail lines)

tick g = do
    clear
    display g
    threadDelay 50000

clear = do
    putStr "\x1b[2J"
    hFlush stdout

main = do
    -- let board2 = (withPiece (pieceAtPos pieceT (3, 3)) board)
    let games = take 25 gamestates
    rs <- sequence (map tick games)
    print rs
