module Wire where

import Network.Socket
import Network
import GHC.IO.Handle
import Data.Word
import Data.ByteString (hGetNonBlocking, empty)
import Data.ByteString.Char8 (unpack)
import Data.List.Split
import Data.Char

import TetrisGame

connect = do
    handle <- connectTo "thomasballinger.com" (PortNumber 4444)
    hPutStr handle "hello!"
    putStrLn "connected"
    return handle

data Msg = BoardView [[Int]]
         | Attack Int
         | Start
         | Leave
         | Defeat
         | Chat String deriving (Show)

data Chatter = Chatter Handle String deriving (Show)

receive :: Chatter -> IO (Maybe Msg, Chatter)
receive (Chatter sock buffer) = do
    bytes <- hGetNonBlocking sock 1024
    let received = buffer ++ unpack bytes in
        if notElem '\n' received
            then return (Nothing, Chatter sock received)
            else return (Just $ msgFromLine $ head (lines received), Chatter sock $ unlines $ tail $ lines received)

receiveMessages :: Chatter -> IO ([Msg], Chatter)
receiveMessages chatter = do
    (msg, newChatter) <- receive chatter
    case msg of
        Nothing -> return ([], newChatter)
        Just m -> do
            (rest, newestChatter) <- receiveMessages newChatter
            return (m:rest, newestChatter)

msgFromLine "Start" = Start
msgFromLine "Leave" = Leave
msgFromLine "Defeat" = Defeat
msgFromLine "Attack1" = Attack 1
msgFromLine "Attack2" = Attack 2
msgFromLine "Attack3" = Attack 3
msgFromLine s = if length s == 200
                    then BoardView (map (map digitToInt) (chunksOf 10 s))
                    else Chat s

boardWire game = boardView game
attackWire n = "attack" ++ show n
startWire = "start"

