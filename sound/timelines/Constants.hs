module Sound.TimeLines.Constants where

import Sound.TimeLines.Types

ionian :: [Signal Value]
ionian = [0, 2, 4, 5, 7, 9, 11]
dorian :: [Signal Value]
dorian = [0, 2, 3, 5, 7, 9, 10]
phrygian :: [Signal Value]
phrygian = [0, 1, 3, 5, 7, 8, 10]
lydian :: [Signal Value]
lydian = [0, 2, 4, 6, 7, 9, 11]
mixolydian :: [Signal Value]
mixolydian = [0, 2, 4, 5, 7, 9, 10]
aeolian :: [Signal Value]
aeolian = [0, 2, 3, 5, 7, 8, 10]
locrian :: [Signal Value]
locrian = [0, 1, 3, 5, 6, 8, 10]


maj :: [Signal Value]
maj = [0, 4, 7]

minor :: [Signal Value]
minor = [0, 3, 7]

maj7 :: [Signal Value]
maj7 = [0, 4, 7, 11]

minor7 :: [Signal Value]
minor7 = [0, 4, 7, 11]
