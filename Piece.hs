module Piece where


data Piece = P { spots :: [ (Int, Int) ],
                 texture :: Int }
                 deriving (Show, Eq)

-- matches GameBoy rotation system
pieceI1 = P [(0, 2), (1, 2), (2, 2), (3, 2)] 1
pieceI2 = P [(2, 0), (2, 1), (2, 2), (2, 3)] 1

pieceO = P [(0, 0), (0, 1), (1, 0), (1, 1)] 2

pieceJ1 = P [(0, 1), (1, 1), (2, 1), (2, 2)] 3
pieceJ2 = P [(0, 2), (1, 2), (1, 1), (1, 0)] 3
pieceJ3 = P [(0, 0), (0, 1), (1, 1), (2, 1)] 3
pieceJ4 = P [(1, 2), (1, 1), (1, 0), (2, 0)] 3

pieceL1 = P [(0, 2), (0, 1), (1, 1), (2, 1)] 4
pieceL2 = P [(0, 0), (1, 0), (1, 1), (1, 2)] 4
pieceL3 = P [(0, 1), (1, 1), (2, 1), (2, 0)] 4
pieceL4 = P [(1, 0), (1, 1), (1, 2), (2, 2)] 4

pieceS1 = P [(0, 2), (1, 2), (1, 1), (2, 1)] 5
pieceS2 = P [(1, 0), (1, 1), (2, 1), (2, 2)] 5

pieceT1 = P [(0, 1), (1, 1), (2, 1), (1, 2)] 6
pieceT2 = P [(1, 0), (1, 1), (2, 1), (1, 2)] 6
pieceT3 = P [(0, 1), (1, 1), (2, 1), (1, 0)] 6
pieceT4 = P [(0, 1), (1, 1), (1, 0), (1, 2)] 6

pieceZ1 = P [(0, 1), (1, 1), (1, 2), (2, 2)] 7
pieceZ2 = P [(1, 2), (1, 1), (2, 1), (2, 0)] 7


rotate :: Piece -> Piece
rotate p = case () of  -- can I split this up?
    () | p == pieceI1 -> pieceI2
       | p == pieceI2 -> pieceI1
       | p == pieceO -> pieceO
       | p == pieceJ1 -> pieceJ2
       | p == pieceJ2 -> pieceJ3
       | p == pieceJ3 -> pieceJ4
       | p == pieceJ4 -> pieceJ1
       | p == pieceL1 -> pieceL2
       | p == pieceL2 -> pieceL3
       | p == pieceL3 -> pieceL4
       | p == pieceL4 -> pieceL1
       | p == pieceS1 -> pieceS2
       | p == pieceS2 -> pieceS1
       | p == pieceT1 -> pieceT2
       | p == pieceT2 -> pieceT3
       | p == pieceT3 -> pieceT4
       | p == pieceT4 -> pieceT1
       | p == pieceZ1 -> pieceZ2
       | p == pieceZ2 -> pieceZ1

psuedoRandomPiece :: Int -> (Piece, Int)
psuedoRandomPiece i = let rn = msprng i in
    (case mod rn 7 of
         0 -> pieceI1
         1 -> pieceO
         2 -> pieceJ1
         3 -> pieceL1
         4 -> pieceS1
         5 -> pieceT1
         6 -> pieceZ1,
     rn)

msprng :: Int -> Int
msprng i = mod (quot (i^2) 1000) 1000000


