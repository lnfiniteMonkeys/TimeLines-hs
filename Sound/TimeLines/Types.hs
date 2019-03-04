module Sound.TimeLines.Types where

import Control.Applicative
import Control.Monad
import Control.Monad.Writer
import Data.List
import qualified Data.Set as Set (toList, fromList)


-- | The type of values sent to synths
type Value = Double

-- | The type passed to Signals
type Time = Double

-- | A section of time between start and end points
type Window = (Time, Time)

windowDur :: Window -> Time
windowDur (s, e) = e - s

-- | The rate at which a Signal is to be sampled
type SampleRate = Int

{- ?
type Scale = [Value] --> used as "Signal Scale"
type Chord = [Value] --> used as "Signal Chord"
type ChordProg = [Chord]
-}

-- | The fundamental type of TimeLines. A Signal of
-- | type "a" is a function from Time to "a"
newtype Signal a = Signal { sampleSig :: Time -> a }

-- | A parameter name and the signal to control it
data ControlSignal = ControlSignal { ctrlParam :: SynthParam
                                   , ctrlSampleRate :: SampleRate
                                   , ctrlSignal :: Signal Value
                                   }

-- | The name of a synth as stored on the server (including SynthDef)
type SynthID = String

-- | The name of a SynthDef's parameter
type SynthParam = String

-- | A list of Param & Signal pairs
data Synth = Synth { synthID :: SynthID
                   , synthSignals :: [ControlSignal]
                   }

-- | A pair of Synths (src, dst)
type Patch = (SynthID, SynthID)

-- | Actions are what define a session
data Action = SynthAction Synth
            | PatchAction Patch
            | ModifierAction SessionModifier
            | EmptyAction

type SessionModifier = Session -> Session

-- | A Collection of objects to be registered along the way
type Collector a = Writer [a] ()

-- | Finite: Window is static and set by user, with optional looping.
-- | Infinite: Window is constantly and infinitely increasing in fixed
-- | increments, with optional resetting.
data SessionMode = FiniteMode | InfiniteMode
  deriving (Eq, Show)

-- | A list of Actions, a Window, and a Mode
data Session = Session { sessStartTime :: Time
                       , sessMode      :: SessionMode
                       , sessActions   :: [Action]
                       , sessWindow    :: Window
                       }

type Scale = [Signal Value]
type Chord = [Signal Value]
type ChordProg = [Chord]

defaultSampleRate :: Int
defaultSampleRate = 1000 

defaultSession :: Session
defaultSession = Session 0 InfiniteMode [] (0, 1)

defaultSignal :: Signal Value
defaultSignal = Signal (\t -> 0)

-- Action Functions --
isSynthAction :: Action -> Bool
isSynthAction (SynthAction _) = True
isSynthAction _ = False

toSynth :: Action -> Synth
toSynth (SynthAction s) = s
toSynth _ = undefined

isPatchAction :: Action -> Bool
isPatchAction (PatchAction _) = True
isPatchAction _ = False

toPatch :: Action -> Patch
toPatch (PatchAction p) = p
toPatch _ = undefined

isSessAction :: Action -> Bool
isSessAction (ModifierAction _) = True
isSessAction _ = undefined

toSessModifier :: Action -> SessionModifier
toSessModifier (ModifierAction a) = a
toSessModifier _ = undefined

-- Session Functions --
-- TODO: refactor this in type class?
synthList :: Session -> [Synth]
synthList = map toSynth . filter isSynthAction . sessActions

patchList :: Session -> [Patch]
patchList = map toPatch . filter isPatchAction . sessActions

sessModifierList :: Session -> [SessionModifier]
sessModifierList = map toSessModifier . filter isSessAction . sessActions

--patchList' sess = (patchList sess) ++ [(s, "mainOut") | s <- unPatchedSynths sess]

--unPatchedSynths :: Session -> [SynthID]
--unPatchedSynths sess = synthIDList sess \\ patchedSynths sess

--patchedSynths sess = removeDups [src | (src, dst) <- patchList' sess]

synthIDList :: Session -> [SynthID]
synthIDList = map synthID . synthList

-- Signal Functions --

-- | Raises any argument to a constant signal of itself
constSig :: a -> Signal a
constSig v = Signal $ \t -> v

-- | The identity Signal, always returns the current time
idSig :: Signal Time
idSig = Signal $ \t -> t
t = idSig

-- Collector Functions --
collectList :: Collector a -> [a]
collectList = execWriter

register :: a -> Collector a
register a = tell [a]

registerEmptyAction :: Collector Action
registerEmptyAction = register EmptyAction

registerSynthAction :: Synth -> Collector Action
registerSynthAction = register . SynthAction

registerPatchAction :: Patch -> Collector Action
registerPatchAction = register . PatchAction

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
  liftA2 f s1 s2 = Signal $ \t -> f (sampleSig s1 t) (sampleSig s2 t)
  sf <*> s = Signal $ \t -> (sampleSig sf t) (sampleSig s t)
  -- A signal with function "f" of type "Time -> (a -> b)", applied at Time "t" to
  -- a signal with function "x" of type "Time -> b", is equal to the value of f for
  -- Time t, applied to the value of x for Time t

-- MONAD
instance Monad Signal where
  return = pure
  (Signal s) >>= f = Signal $ \t -> let firstResult = s t
                                        sigB = f firstResult
                                    in  sampleSig sigB t

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
