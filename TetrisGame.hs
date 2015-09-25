module TetrisGame where

import Piece

data Game = G { board :: [[Int]],
                piece :: Piece,
                location :: (Int, Int),
                seed :: Int}
                deriving (Show, Eq)

newGame = do
    let b = [replicate 10 0 | x <- [1..20]]
    G b pieceT1 (3, 3) 12345

boardView game = withPiece (activePiece game) (board game)

activePiece game = pieceAtPos (piece game) (location game)

freezePiece game = let (newPiece, newSeed) = psuedoRandomPiece (seed game) in
                       G (boardView game) newPiece (0, 0) newSeed

pieceFits :: Game -> Bool
pieceFits game =
    let p = activePiece game in
        not (any (spotBad (board game)) (spots p))

spotFilled board (x, y) = ((board!!y)!!x) /= 0
spotExists board (x, y) = (y < length board) && x < length (head board) &&
                           y > 0-1 && x > -1
spotBad board spot = not (spotExists board spot) || spotFilled board spot

dropPiece :: Game -> Game
dropPiece g =
    let (x, y) = location g in
        let newG = G (board g) (piece g) (x, y+1) (seed g) in
            if pieceFits newG
            then newG
            else removeLines (freezePiece g)

isFull :: [Int] -> Bool
isFull line = notElem 0 line

notFull line = elem 0 line

removeLines :: Game -> Game
removeLines (G b p l s) =
    let left = filter notFull b in
        let newLines = (replicate (length b - length left)
                               (replicate (length (head b)) 0)) in
            G (newLines ++ left) p l s

movePiece :: Game -> Int -> Game
movePiece g dx =
    let (x, y) = location g in
        let newG = G (board g) (piece g) (x + dx, y) (seed g) in
            if pieceFits newG
            then newG
            else g

rotatedPiece :: Game -> Game
rotatedPiece (G b p l s) = G b (rotate p) l s

rotatePiece :: Game -> Game
rotatePiece g = if pieceFits (rotatedPiece g)
                then rotatedPiece g
                else g

withBlock t (x, y) board =
    [if fst l == y
     then [if fst m == x
           then t
           else (snd m) | m <- zip [0..] (snd l)]
     else (snd l) | l <- (zip [0..] board)]

withPiece piece board = foldr (withBlock (texture piece)) board (spots piece)

offset (dx, dy) (x, y) = (dx + x, dy + y)

pieceAtPos :: Piece -> (Int, Int) -> Piece
pieceAtPos p delta = P (map (offset delta) (spots p)) (texture p)

gameBoardWithPiece game = do
    let ((piece, (dx, dy)), board) = game
    let placedPiece = [(x+dx, y+dy) | (x, y) <- piece]
    1

gameTick :: Game -> Char -> Game
gameTick game 'a' = movePiece game (0-1)
gameTick game 'd' = movePiece game 1
gameTick game 'e' = rotatePiece game
gameTick game 'q' = rotatePiece game
gameTick game _ = dropPiece game

gamestates = iterate dropPiece newGame
