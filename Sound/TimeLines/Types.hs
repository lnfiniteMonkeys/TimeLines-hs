module Sound.TimeLines.Types where

import Control.Applicative
import Control.Monad
import Control.Monad.Writer

--import qualified Data.Map.Strict as Map

-- | The type actually written to files
-- | (default = Double)
type Value = Double

-- | The type passed to Signals
type Time = Double

-- | A section of time between start and end points
type Window = (Time, Time)

windowDur :: Window -> Time
windowDur (s, e) = e - s

-- | The rate at which a Signal is to be sampled
type SamplingRate = Int

{- ?
type Scale = [Value] --> used as "Signal Scale"
type Chord = [Value] --> used as "Signal Chord"
type ChordProg = [Chord]
-}

type Scale = [Signal Value]
type Chord = [Signal Value]
type ChordProg = [Chord]

-- | The name of a synth as stored on the server (including SynthDef)
type SynthID = String

type SynthGroup = String

-- | The name of a SynthDef's parameter
type Param = String

-- | The fundamental type of TimeLines. A Signal of
-- | type "a" is a function from Time to "a"
newtype Signal a = Signal {runSig :: Time -> a}

-- | A parameter name and the signal to control it
type ControlSignal = (Signal Value, SamplingRate)

-- | A combination of a Control Signal and a Parameter to control
type ParamSignal = (Param, ControlSignal)

-- | A list of Param & Signal pairs
type Synth = [ParamSignal]

-- | A SynthID & Synth pair
type SynthWithID = (SynthID, Synth)

-- | A pair of Synths (Output, Input)
type Patch = (SynthID, SynthID)

-- | An Action can either be a Synth or a Path
data Action = ActionSynth SynthWithID
            | ActionPatch Patch
            | EmptyAction

-- | A Collection of objects to be registered along the way
type Collector a = Writer [a] ()

-- | Finite: Window is static and set by user, with optional looping.
-- | Infinite: Window is constantly and infinitely increasing in fixed
-- | increments, with optional resetting.
data SessionMode = FiniteMode | InfiniteMode
  deriving (Eq, Show)

-- | A list of Actions, a Window, and a Mode
data Session = Session {actions::[Action],
                        sessionWindow::Window,
                        sessionMode::SessionMode
                       }

-- | Everything needed to write a Param control buffer
data FiniteTimeLine = FTL {ftlParamSig::ParamSignal,
                           ftlWindow::Window
                          }

-- Defaults --
defaultSamplingRate = 700 

defaultSession :: Session
defaultSession = Session [] (0, 1) FiniteMode

defaultSignal :: Signal Value
defaultSignal = Signal (\t -> 0)


-- Action Functions --
isSynth :: Action -> Bool
isSynth (ActionSynth _) = True
isSynth _ = False

toSynth :: Action -> SynthWithID
toSynth (ActionSynth s) = s
toSynth _ = undefined

isPatch :: Action -> Bool
isPatch (ActionPatch _) = True
isPatch _ = False

toPatch :: Action -> Patch
toPatch (ActionPatch p) = p
toPatch _ = undefined


-- Session Functions --
synthList :: Session -> [SynthWithID]
synthList = map toSynth . filter isSynth . actions

patchList :: Session -> [Patch]
patchList = map toPatch . filter isPatch . actions

synthIDList :: Session -> [SynthID]
synthIDList = map fst . synthList

-- FiniteTimeLine functions --
ftlSR :: FiniteTimeLine -> SamplingRate
ftlSR (FTL (_, (_, sr)) _) = sr


-- Signal Functions --

-- | The identity Signal, always returns the current time
t :: Signal Value
t = Signal $ \t -> t

-- | Raises any argument to a constant signal of itself
constSig :: a -> Signal a
constSig v = Signal $ \t -> v


-- Collector Functions --
collectList :: Collector a -> [a]
collectList = execWriter

register :: a -> Collector a
register a = tell [a]

registerEmptyAction :: Collector Action
registerEmptyAction = register EmptyAction

registerSynthAction :: SynthWithID -> Collector Action
registerSynthAction = register . ActionSynth

registerPatchAction :: Patch -> Collector Action
registerPatchAction = register . ActionPatch

registerParam :: ParamSignal -> Collector ParamSignal
registerParam = register

  ---------------------------INSTANCES---------------------------
-- FUNCTOR
instance Functor Signal where
  fmap f (Signal sf) = Signal $ fmap f sf
  --Alternate implementation:
  --fmap f (Signal sf) = Signal $ \t -> f (sf t)
  (<$) = fmap . const

-- APPLICATIVE
instance Applicative Signal where
  -- Transform a value "a" to a signal of constant value "a"
  pure = constSig
  liftA2 f s1 s2 = Signal $ \t -> f (runSig s1 t) (runSig s2 t)
  sf <*> s = Signal $ \t -> (runSig sf t) (runSig s t)
  -- A signal with function "f" of type "Time -> (a -> b)", applied at Time "t" to
  -- a signal with function "x" of type "Time -> b", is equal to the value of f for
  -- Time t, applied to the value of x for Time t

-- MONAD
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

-- NUM
instance (Num a, Eq a) => Num (Signal a) where
  negate      = fmap negate
  (+)         = liftA2 (+)
  (*)         = liftA2 lazyMul -- lazy multiplicator
  fromInteger = pure . fromInteger
  abs         = fmap abs
  signum      = fmap signum


-- FRACTIONAL
instance (Fractional a, Eq a) => Fractional (Signal a) where
  fromRational = pure . fromRational
  recip = fmap recip

-- FLOATING
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

