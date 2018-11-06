module Sound.TimeLines.Types
  (  Signal(..)
    ,Time
    ,Value
    ,Window
    ,Chord
    ,Scale
    ,ChordProg
    ,SynthID
    ,Synth
    ,SynthWithID(..)
    ,Param
    ,ControlSignal
    ,ParamSignal
    ,SamplingRate
    ,FiniteTimeLine(..)
    ,Session(..)
    ,ftlSR
    ,globalSessionRef
    ,globalWindowRef
--    ,defaultFTL
    ,defaultSamplingRate
    ,t
    ,constSig
    ,lazyMul
  ) where

import Prelude as Pr
import Control.Applicative
import Control.Monad
import Data.Map.Strict as Map
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import Control.Concurrent (forkIO, ThreadId)
import Control.Concurrent.Async (mapConcurrently)
import Control.Monad.Writer


-- | The type actually written to files
-- | (default = Double)
type Value = Double

-- | The type passed to Signals
type Time = Double

-- | Type representing a section of time
-- | with start and end points
type Window = (Time, Time)

-- | Global reference of the time Window over which
-- | to render each TimeLine
{-# NOINLINE globalWindowRef #-}
-- the "NOINLINE" statement makes sure to never
-- replace globalWindowRef with its body, thus creating
-- another IORef
globalWindowRef :: IORef Window
globalWindowRef = unsafePerformIO $ newIORef (0, 1)

-- | Represents the rate at which a Signal is to be sampled
type SamplingRate = Int
-- | Represents the name of a synth stored on the server
type SynthID = String
-- | Represents the name of a SynthDef's parameter
type Param = String
-- | A parameter name and the signal to control it
type ControlSignal = (Signal Value, SamplingRate)
-- | Every synth has one of those, a list of parameters and signals
--type ParamMap = Map.Map Param ControlSignal
-- | A synth with a name and a map of signals
type ParamSignal = (Param, ControlSignal)

type Synth = [ParamSignal]

type SynthWithID = (SynthID, Synth)

data SessionMode = Static | Dynamic
  deriving Show

data Session = Session {synthList::[SynthWithID],
                        sessionWindow::Window,
                        sessionMode::SessionMode
                       }

data FiniteTimeLine = FTL {ftlParamSig::ParamSignal,
                           ftlWindow::Window
                          }

ftlSR :: FiniteTimeLine -> SamplingRate
ftlSR (FTL (_, (_, sr)) _) = sr

-- | The default sampling rate
defaultSamplingRate = 700 

-- | Default empty session
defaultSession :: Session
defaultSession = Session [] (0, 1) Static

-- | A global reference to the current session
{-# NOINLINE globalSessionRef #-}
globalSessionRef :: IORef Session
globalSessionRef = unsafePerformIO $ newIORef defaultSession


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
constSig v = Signal $ \t -> v


---------------------------INSTANCES---------------------------

--FUNCTOR
instance Functor Signal where
  fmap f (Signal sf) = Signal $ fmap f sf
  --Alternate implementation:
  --fmap f (Signal sf) = Signal $ \t -> f (sf t)
  (<$) = fmap . const

--APPLICATIVE
instance Applicative Signal where
  -- Transform a value "a" to a signal of constant value "a"
  pure = constSig
  liftA2 f s1 s2 = Signal $ \t -> f (runSig s1 t) (runSig s2 t)
  sf <*> s = Signal $ \t -> (runSig sf t) (runSig s t)
  -- A signal with function "f" of type "Time -> (a -> b)", applied at Time "t" to
  -- a signal with function "x" of type "Time -> b", is equal to the value of f for
  -- Time t, applied to the value of x for Time t

--MONAD
instance Monad Signal where
  return = pure
  (Signal s) >>= f = Signal $ \t -> let firstResult = s t
                                        sigB = f firstResult
                                    in  runSig sigB t

-- | Lazy multiplicator, doesn't evauate the second
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

