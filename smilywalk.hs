import System.IO

color :: String -> String
color "red" = "\x1b[31m"
color "yellow" = "\x1b[33m"
color _ = ""

colored :: String -> String -> String
colored c s = color(c) ++ s ++ "\x1b[37m"

clear = "\x1b[2J"
saveCursor = "\x1b[s"
restoreCursor = "\x1b[s"
hideCursor = "\x1b[?25l"
showCursor = "\x1b[?25h"

strAtSpot x y s = "\x1b[" ++ (show $ y + 1 ) ++ ";" ++ (show (x + 1)) ++ "H" ++ s
putStrAtSpot x y s = putStr $ saveCursor ++ (strAtSpot x y s) ++ restoreCursor


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
    putStr clear
    putStrAtSpot x y (colored "red" face)
    hFlush stdout
    c <- getChar
    mainloop (moveX c x) (moveY c y)

main = do
    putStrLn $ colored "red" "type things!"
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    putStr hideCursor
    mainloop 1 0
