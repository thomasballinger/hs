module Terminal where

import System.IO



color :: String -> String
color "red" = "\x1b[41m"
color "green" = "\x1b[42m"
color "yellow" = "\x1b[43m"
color "blue" = "\x1b[44m"
color "magenta" = "\x1b[45m"
color "cyan" = "\x1b[46m"
color "white" = "\x1b[47m\x1b[30m"
color _ = ""

colored :: String -> String -> String
colored c s = color c ++ s ++ "\x1b[0m"

saveCursor = "\x1b[s"
restoreCursor = "\x1b[s"
hideCursor = "\x1b[?25l"
showCursor = "\x1b[?25h"

strAtSpot x y s = "\x1b[" ++ show ( y + 1 ) ++ ";" ++ show (x + 1) ++ "H" ++ s
putStrAtSpot x y s = putStr $ saveCursor ++ strAtSpot x y s ++ restoreCursor

newScreen = do
    putStr "\x1b[2J"
    hFlush stdout

cursorToBottomLeft = do
    putStr "\x1b[1000D"
    putStr "\x1b[1000B"
    hFlush stdout

cursorUp n = do
    putStr $ "\x1b[" ++ show n ++ "A"
    hFlush stdout

moveX :: Char -> Integer -> Integer
moveX 'h' x = x - 1
moveX 'l' x = x + 1
moveX _ x = x
moveY :: Char -> Integer -> Integer
moveY 'j' y = y + 1
moveY 'k' y = y - 1
moveY _ y = y

face = "(◕‿◕)"

mainloop x y = do
    putStrAtSpot x y (colored "red" face)
    hFlush stdout
    c <- getChar
    mainloop (moveX c x) (moveY c y)

stuff = do
    putStrLn $ colored "red" "type things!"
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    putStr hideCursor
    mainloop 1 0
