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
    let b = [take 10 (repeat 0) | x <- [1..10]]
    G b pieceT (3, 3)

boardView game = withPiece (activePiece game) (board game)

activePiece game = pieceAtPos (piece game) (location game)

freezePiece game = G (boardView game) pieceT (0, 0)

pieceFits :: Game -> Bool
pieceFits game =
    let p = activePiece game in
        not (any (spotBad (board game)) (spots p))


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

withBlock (x, y) board =
    [if (fst l) == y
     then [if (fst m) == x
           then 1
           else (snd m) | m <- zip [0..] (snd l)]
     else (snd l) | l <- (zip [0..] board)]

withPiece piece board = foldr withBlock board (spots piece)

offset (dx, dy) (x, y) = (dx + x, dy + y)

pieceAtPos :: Piece -> (Int, Int) -> Piece
pieceAtPos p delta = P (map (offset delta) (spots p)) (texture p)

gameBoardWithPiece game = do
    let ((piece, (dx, dy)), board) = game
    let placedPiece = [(x+dx, y+dy) | (x, y) <- piece]
    1

gamestates = iterate dropPiece newGame
