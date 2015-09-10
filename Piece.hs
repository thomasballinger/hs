module Piece where


data Piece = P { spots :: [ (Int, Int) ],
                 texture :: Int }
                 deriving (Show, Eq)

pieceT1 = P [(0, 1), (1, 1), (2, 1), (1, 2)] 1
pieceT2 = P [(1, 0), (1, 1), (2, 1), (1, 2)] 1
pieceT3 = P [(0, 1), (1, 1), (2, 1), (1, 0)] 1
pieceT4 = P [(0, 1), (1, 1), (1, 0), (1, 2)] 1

pieceL1 = P [(0, 2), (0, 1), (1, 1), (2, 1)] 2  -- matches GameBoy rotation system
pieceL2 = P [(0, 0), (1, 0), (1, 1), (1, 2)] 2
pieceL3 = P [(0, 1), (1, 1), (2, 1), (2, 0)] 2
pieceL4 = P [(1, 0), (1, 1), (1, 2), (2, 2)] 2

pieceO = P [(0, 0), (0, 1), (1, 0), (1, 1)] 3

rotate :: Piece -> Piece
rotate p = case () of
    () | p == pieceT1 -> pieceT2
       | p == pieceT2 -> pieceT3
       | p == pieceT3 -> pieceT4
       | p == pieceT4 -> pieceT1
       | p == pieceL1 -> pieceL2  -- can I split this up?
       | p == pieceL2 -> pieceL3
       | p == pieceL3 -> pieceL4
       | p == pieceL4 -> pieceL1
       | p == pieceO -> pieceO
       | otherwise -> pieceT1

psuedoRandomPiece :: Int -> (Piece, Int)
psuedoRandomPiece i = let rn = msprng i in
    (case mod rn 3 of
         0 -> pieceT1
         1 -> pieceL1
         2 -> pieceO,
     rn)

msprng :: Int -> Int
msprng i = mod (quot (i^2) 1000) 1000000


