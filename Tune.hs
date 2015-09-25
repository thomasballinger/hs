module Tune where

import Text.Regex (splitRegex, mkRegex)
import Text.Regex.Posix
import Sound.ALUT

data Note = N { note :: String,
                duration :: Float }
                deriving (Show, Eq)

-- Mi Sol Tiiii
s = "Ti  Fa Sol La  Sol Fa Mi  Mi Sol Ti  La Sol Fa   Sol La  Ti  Sol  Mi  Mi      La  Do2 Mi2  Re2 Do2 Ti   Sol Ti  La Sol Fa  Fa Sol La  Ti  Sol  Mi  Mi    "

parseSong :: String -> [Note]
parseSong s =
    let words = getAllTextMatches $ s =~ "[A-Z][a-z0-9]+[ ]+" :: [String] in
        [N (word =~ "[A-Z][a-z0-9]+" :: String) (fromInteger (toInteger (length (word =~ "[ ]+" :: String)))) | word <- words]

solfegeToFreq :: String -> Float
solfegeToFreq "Do" = 2  ** (12.0/12.0)
solfegeToFreq "Re" = 2  ** (14.0/12.0)
solfegeToFreq "Mi" = 2  ** (16.0/12.0)
solfegeToFreq "Fa" = 2  ** (18.0/12.0)
solfegeToFreq "Sol" = 2 ** (19.0/12.0)
solfegeToFreq "La" = 2  ** (21.0/12.0)
solfegeToFreq "Ti" = 2  ** (23.0/12.0)
solfegeToFreq "Do2" = 2 ** (24.0/12.0)
solfegeToFreq "Re2" = 2 ** (26.0/12.0)
solfegeToFreq "Mi2" = 2 ** (28.0/12.0)
solfegeToFreq "Fa2" = 2 ** (29.0/12.0)
solfegeToFreq "Sol2" = 2** (31.0/12.0)
solfegeToFreq "La2" = 2 ** (33.0/12.0)
solfegeToFreq "Ti2" = 2 ** (35.0/12.0)

toneFromSolfege n = Sine (solfegeToFreq (note n) * 50) 0 (duration n / 5.0)

sinesFromSong song = [toneFromSolfege n | n <- parseSong song]
