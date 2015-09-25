import Control.Exception
import Control.Concurrent
import Control.Monad
import System.IO

import TetrisGame
import Terminal
import Music
import Wire

data TwoPlayerGame = TwoPlayerGame { game :: Game,
                                     chatter :: Chatter,
                                     chat :: [String],
                                     oppBoard :: [[Int]] }
                                     deriving (Show)


blockDisplay n = ("   " : [colored color "xxx" | color <- ["red", "yellow", "green", "blue", "magenta", "cyan", "white"]])!!n

display tpg = boardDisplay2 (boardView (game tpg)) (oppBoard tpg)

boardDisplay board = do
    putStrLn ("\n" ++ "+" ++ replicate 30 '-' ++ "+")
    displayLines (lineStrings board)
    putStr $ "+" ++ replicate 30 '-' ++ "+"
    hFlush stdout

boardDisplay2 board1 board2 = displayLines
     (["          Your board:                    Opponent's board:"] ++
      ["+" ++ replicate 30 '-' ++ "+ +" ++ replicate 30 '-' ++ "+"] ++
      vertcatBoardLines (lineStrings board1) (lineStrings board2) ++
      ["+" ++ replicate 30 '-' ++ "+ +" ++ replicate 30 '-' ++ "+"])

vertcatBoardLines lines1 lines2 = map (\(x, y) -> x ++ " " ++ y) $ zip lines1 lines2

lineStrings :: [[Int]] -> [[Char]]
lineStrings lines =
    if null lines
        then
            []
        else
            let line = head lines in
                ["|" ++ concat [blockDisplay x | x <- line] ++ "|",
                 "|" ++ concat [blockDisplay x | x <- line] ++ "|"] ++
                lineStrings (tail lines)

displayLines :: [String] -> IO()
displayLines lines = do
    cursorToBottomLeft
    cursorUp (length lines)
    mapM_ putStrLn lines

tick :: TwoPlayerGame -> Int -> IO TwoPlayerGame
tick tpg i = do
    display tpg
    inputReady <- hWaitForInput stdin 300
    c <- if inputReady then getChar else return ' '
    let newgame = gameTick (game tpg) c
    (messages, newChatter) <- receiveMessages (chatter tpg)
    let newtpg = (foldl processMsg
                        (TwoPlayerGame newgame newChatter (chat tpg) (oppBoard tpg))
                        messages)
    publish newtpg
    return newtpg

processMsg :: TwoPlayerGame -> Msg -> TwoPlayerGame
processMsg tpg (Attack n) = tpg
processMsg tpg Start = tpg
processMsg tpg Leave = tpg
processMsg tpg Defeat = tpg
processMsg (TwoPlayerGame g s c o) (Chat msg) = TwoPlayerGame g s (msg:c) o
processMsg (TwoPlayerGame g s c _) (BoardView b) = TwoPlayerGame g s c b


publish :: TwoPlayerGame -> IO ()
publish (TwoPlayerGame g (Chatter s b) c o) = hPutStrLn s $ concatMap (concatMap show) (boardView g)

newTwoPlayerGame sock =
    TwoPlayerGame newGame (Chatter sock "") ["no chat yet"] (replicate 20 [1, 2, 0, 3, 4, 5, 0, 6, 7])

mainIO sock = foldM_ tick (newTwoPlayerGame sock) [0..]

main = do
    putStrLn $ colored "red" "Let's play Tetris!"
    sock <- connect
    startMusic
    bracket_
        (do
            hSetBuffering stdin NoBuffering
            hSetEcho stdin False
            newScreen
            )
        (do
            hSetBuffering stdin LineBuffering
            hSetEcho stdin True
            putStrLn "bye"
            )
        (mainIO sock)

