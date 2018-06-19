module Sound.TimeLines.Types
  (  Signal(..)
    ,Time
    ,Value
    ,Window
    ,TLinfo(..)
    ,TimeLine(..)
    ,Param
    ,SynthID
    ,infNumFrames
    ,defaultInfo
  ) where

import Prelude as Pr

type Value = Double
type Time = Double
type Window = (Time, Time)

newtype Signal = Signal {sigFunc :: Time -> Value}

-- A timeline (file written to disk) has a start (> 0) and end point,
-- a samplerate and a string that denotes which parameter of the synth it controls
data TLinfo = TLinfo {infWindow::Window,
                      infSR::Int,
                      infParam::String
                     }
              deriving (Eq, Show)

-- Duration of file to be written and played back
infDur :: TLinfo -> Time
infDur (TLinfo (s, e) _ _) = e - s

-- The number of frames that will be written to file
infNumFrames :: TLinfo -> Int
infNumFrames i = Pr.floor $ infDur i * fromIntegral (infSR i)

--make it an IORef?
defaultSampleRate = 700
defaultInfo :: Window -> String -> TLinfo
defaultInfo w argName = TLinfo w defaultSampleRate argName

-- A TimeLine is made up of a signal (defined over infinite time starting from 0)
-- and a TLinfo, which determines which part of the signal we are observing
data TimeLine = TimeLine {tlSig::Signal,
                          tlInfo::TLinfo
                         }



type SynthID = String
type Param = String
type ParamList = [Param]


-- don't evaluate second term if first term is 0
(*.) :: (Num a, Eq a) => a -> a -> a
(*.) 0 _ = 0
(*.) a b = a * b

--shortcut to use with $ for RHS argument
mu = (*.)

