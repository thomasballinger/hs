module Music where

import Control.Monad
import Sound.ALUT

import Tune

-- code from http://bluishcoder.co.nz/articles/haskell/openal.html
-- thanks CHRIS DOUBLE

playMusic :: IO ()
playMusic =
  withProgNameAndArgs runALUTUsingCurrentContext $ \_ _ ->
  do
    (Just device) <- openDevice Nothing
    (Just context) <- createContext device []
    currentContext $= Just context
    buffers <- mapM createBuffer (sinesFromSong s)
    [source] <- genObjectNames 1
    queueBuffers source buffers
    play [source]
    sleep 6.2
    closeDevice device
    return ()

-- main = playSound
