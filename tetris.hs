import Control.Concurrent
import System.IO

pieceT = P [(0, 1), (1, 1), (2, 1), (1, 2)]
pieceL = P [(0, 2), (0, 1), (0, 0), (1, 0)]
pieceO = P [(0, 0), (0, 1), (1, 0), (1, 1)]

data Piece = P [ (Int, Int) ] deriving (Show, Eq)

data Game = G { board :: [[Int]],
                piece :: Piece,
                location :: (Int, Int)}

newGame = do
    let b = [take 10 (repeat 0) | x <- [1..10]]
    G b pieceT (3, 3)

boardView game = withPiece (activePiece game) (board game)

activePiece game = pieceAtPos (piece game) (location game)

freezePiece game = G (boardView game) pieceT (0, 0)

pieceFits :: Game -> Bool
pieceFits game =
    let (P spots) = activePiece game in
        not (any (spotBad (board game)) spots)


spotFilled board (x, y) = (((board!!y)!!x) /= 0)
spotExists board (x, y) = (y < length board) && (x < (length (head board)))
spotBad board spot = (not (spotExists board spot)) || spotFilled board spot


dropPiece :: Game -> Game
dropPiece g =
    let (x, y) = location g in
        let newG = G (board g) (piece g) (x, y+1) in
            if pieceFits newG
            then newG
            else freezePiece g

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
                    putStrLn [if x == 0 then ' ' else 'x' | x <- line]
                    displayLines (tail lines)

withBlock (x, y) board =
    [if (fst l) == y
     then [if (fst m) == x
           then 1
           else (snd m) | m <- zip [0..] (snd l)]
     else (snd l) | l <- (zip [0..] board)]

withPiece (P spots) board = foldr withBlock board spots

offset (dx, dy) (x, y) = (dx + x, dy + y)

pieceAtPos :: Piece -> (Int, Int) -> Piece
pieceAtPos (P xs) delta = P $ map (offset delta) xs

gameBoardWithPiece game = do
    let ((piece, (dx, dy)), board) = game
    let placedPiece = [(x+dx, y+dy) | (x, y) <- piece]
    1

gamestates = iterate dropPiece newGame

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
