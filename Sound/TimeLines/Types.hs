module Sound.TimeLines.Types
  (  Signal(..)
    ,Time
    ,Value
    ,Window
    ,Chord
    ,Scale
    ,ChordProg
    ,TLinfo(..)
    ,TimeLine(..)
    ,SynthID
    ,Param
    ,infNumFrames
    ,defaultInfo
    ,t
    ,constSig
  ) where

import Prelude as Pr
import Control.Applicative
import Control.Monad


-- | The type actually written to files
-- | (default = Double)
type Value = Double

-- | The type passed to Signals
type Time = Double

-- | Type representing a section of time
-- | with start and end points
type Window = (Time, Time)

-- | Represents the name of a synth stored on the server
type SynthID = String
-- | Represents the name of a SynthDef's parameter
type Param = String

{- ?
type Scale = [Value] --> used as "Signal Scale"
type Chord = [Value] --> used as "Signal Chord"
type ChordProg = [Chord]
-}

type Scale = [Signal Value]
type Chord = [Signal Value]
type ChordProg = [Chord]

-- | The fundamental type of TimeLines.
-- | A Signal of type "a" is a function
-- | from Time to "a"
newtype Signal a = Signal {runSig :: Time -> a}

-- | The time Signal, always returns the time
t :: Signal Value
t = Signal $ \t -> t

-- | Takes any argument and raises it to
-- | a Signal that always returns that argument
constSig :: a -> Signal a
constSig a = Signal $ \t -> a


---------------------------INSTANCES---------------------------

--FUNCTOR
instance Functor Signal where
  fmap f (Signal a) = Signal $ fmap f a
  (<$) = fmap . const

--APPLICATIVE
instance Applicative Signal where
  -- Transform a value "a" to a signal of constant value "a"
  pure = constSig
  sf <*> s = Signal $ \t -> (runSig sf t) (runSig s t)
  --liftA2 f s1 s2 = Signal $ \t -> f (runSig s1 t) (runSig s2 t)
  -- A signal with function "f" of type "Time -> (a -> b)", applied at Time "t" to
  -- a signal with function "x" of type "Time -> b", is equal to the value of f for
  -- Time t, applied to the value of x for Time t

--MONAD
instance Monad Signal where
  return = pure
  (Signal s) >>= f = Signal $ \t -> let newA = s t
                                        sigB = f newA
                                    in  runSig sigB t

-- | Lazy multiplicator, doesn't evaluate the second
-- | term if first term is equal to 0
lazyMul :: (Num a, Eq a) => a -> a -> a
lazyMul 0 _ = 0
lazyMul a b = a * b

-- | Shortcut for being able to use
-- | "$" with the RHS expression
mul = lazyMul

--NUM
instance (Num a, Eq a) => Num (Signal a) where
  negate      = fmap negate
  (+)         = liftA2 (+)
  (*)         = liftA2 lazyMul -- lazy multiplicator
  fromInteger = pure . fromInteger
  abs         = fmap abs
  signum      = fmap signum


--FRACTIONAL
instance (Fractional a, Eq a) => Fractional (Signal a) where
  fromRational = pure . fromRational
  recip = fmap recip

--FLOATING
instance (Floating a, Eq a) => Floating (Signal a) where
  pi    = pure pi
  sqrt  = fmap sqrt
  exp   = fmap exp
  log   = fmap log
  sin   = fmap sin
  cos   = fmap cos
  asin  = fmap asin
  atan  = fmap atan
  acos  = fmap acos
  sinh  = fmap sinh
  cosh  = fmap cosh
  asinh = fmap asinh
  atanh = fmap atanh
  acosh = fmap acosh

-- | A TLinfo holds all the information needed
-- | to sample a Signal, such as the Window, the
-- | sampling rate, and a string denoting which
-- | parameter of the synth it controls
data TLinfo = TLinfo {infWindow::Window,
                      infSR::Int,
                      infParam::String
                     }
              deriving (Eq, Show)

-- | Calculates the duration, in seconds, of a TLinfo
infDur :: TLinfo -> Time
infDur (TLinfo (s, e) _ _) = e - s

-- | Calculates the number of frames that will be sampled
-- | and written to file
infNumFrames :: TLinfo -> Int
infNumFrames i = Pr.floor $ infDur i * fromIntegral (infSR i)

-- | The default sampling rate
defaultSampleRate = 700 -- make it an IORef?

-- | Returns the a default TLinfo given a window and a Param
defaultInfo :: Window -> Param -> TLinfo
defaultInfo w p = TLinfo w defaultSampleRate p

{-|
A TimeLine is the result of observing and sampling
a Signal over a certain Window, what actually gets
written to disk. It is made up of a Signal and an
accompanying TLinfo.
-}
data TimeLine a = TimeLine {tlSig :: Signal a,
                            tlInfo :: TLinfo
                           }

