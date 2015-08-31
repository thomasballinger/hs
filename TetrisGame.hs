module TetrisGame where

pieceT = P [(0, 1), (1, 1), (2, 1), (1, 2)] 1
pieceL = P [(0, 2), (0, 1), (0, 0), (1, 0)] 2
pieceO = P [(0, 0), (0, 1), (1, 0), (1, 1)] 3

data Piece = P { spots :: [ (Int, Int) ],
                 texture :: Int }
                 deriving (Show, Eq)

data Game = G { board :: [[Int]],
                piece :: Piece,
                location :: (Int, Int)}

newGame = do
    let b = [replicate 10 0 | x <- [1..20]]
    G b pieceT (3, 3)

boardView game = withPiece (activePiece game) (board game)

activePiece game = pieceAtPos (piece game) (location game)

freezePiece game = G (boardView game) pieceL (0, 0)

pieceFits :: Game -> Bool
pieceFits game =
    let p = activePiece game in
        not (any (spotBad (board game)) (spots p))

spotFilled board (x, y) = ((board!!y)!!x) /= 0
spotExists board (x, y) = (y < length board) && x < length (head board) &&
                           y > (0-1) && x > (0-1)
spotBad board spot = not (spotExists board spot) || spotFilled board spot

dropPiece :: Game -> Game
dropPiece g =
    let (x, y) = location g in
        let newG = G (board g) (piece g) (x, y+1) in
            if pieceFits newG
            then newG
            else freezePiece g

movePiece :: Game -> Int -> Game
movePiece g dx =
    let (x, y) = location g in
        let newG = G (board g) (piece g) (x + dx, y) in
            if pieceFits newG
            then newG
            else g

rotatedPiece :: Game -> Game
rotatedPiece (G b (P _ 1) l) = G b pieceL l
rotatedPiece (G b (P _ 2) l) = G b pieceO l
rotatedPiece (G b (P _ 3) l) = G b pieceT l
rotatedPiece g = g

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
