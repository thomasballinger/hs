
pieceT = P [(0, 1), (1, 1), (2, 1), (1, 2)]
pieceL = P [(0, 2), (0, 1), (0, 0), (1, 0)]
pieceO = P [(0, 0), (0, 1), (1, 0), (1, 1)]

data Piece = P [ (Int, Int) ] deriving (Show, Eq)

data Game = G { board :: [String],
                piece :: Piece,
                location :: (Int, Int)}

dropPiece :: Game -> Game
dropPiece g =
    let (x, y) = location g in
        G (board g) (piece g) (x, y+1)


display board = do
    putStrLn (take 10 (repeat '-'))
    displayLines board

displayLines lines = do
    if null lines
        then putStrLn (take 10 (repeat '-'))
        else do
            putStrLn (head lines)
            displayLines (tail lines)

withBlock x y board =
    [if (fst l) == y
     then [if (fst m) == x
           then 'x'
           else (snd m) | m <- zip [0..] (snd l)]
     else (snd l) | l <- (zip [0..] board)]

withBlockInSpot xy board = withBlock (fst xy) (snd xy) board
withPiece (P spots) board = foldr withBlockInSpot board spots

-- [[0,0,0],
--  [0,0,0],
--  [0,0,0]]


offset (dx, dy) (x, y) = (dx + x, dy + y)

pieceAtPos :: Piece -> (Int, Int) -> Piece
pieceAtPos (P xs) delta = P $ map (offset delta) xs

gameBoardWithPiece game = do
    let ((piece, (dx, dy)), board) = game
    let placedPiece = [(x+dx, y+dy) | (x, y) <- piece]
    1

main = do
    let b = [take 10 (repeat ' ') | x <- [1..10]]
    display b
    -- let board2 = (withPiece (pieceAtPos pieceT (3, 3)) board)
    let game = G b pieceT (3, 3)
    display (withPiece (pieceAtPos (piece game) (location game)) (board game))
    let game2 = dropPiece game
    display (withPiece (pieceAtPos (piece game2) (location game2)) (board game2))
    putStrLn ""

