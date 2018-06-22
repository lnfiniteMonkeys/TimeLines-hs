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
    ,t
  ) where

import Prelude as Pr
import Control.Applicative
import Control.Monad

type Value = Double
type Time = Double
type Window = (Time, Time)

newtype Signal a = Signal {runSig :: Time -> a}

---------------INSTANCES---------------
--FUNCTOR
instance Functor Signal where
  fmap f (Signal a) = Signal $ fmap f a
  (<$) = fmap . const

--APPLICATIVE
instance Applicative Signal where
  -- Transform a value "a" to a signal of constant value "a"
  pure a = Signal $ \t -> a
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

instance (Num a, Eq a) => Num (Signal a) where
      negate      = fmap negate
      (+)         = liftA2 (+)
      (*)         = liftA2 (*) -- lazy multiplicator
      fromInteger = pure . fromInteger
      abs         = fmap abs
      signum      = fmap signum


instance (Fractional a, Eq a) => Fractional (Signal a) where
  fromRational = pure . fromRational
  recip = fmap recip

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


--instance (Num a, Ord a) => Real (Signal a) where
--  toRational = 


t :: Signal Value
t = Signal $ \t -> t


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
-- and a cTLinfo, which determines which part of the signal we are observing
data TimeLine a = TimeLine {tlSig::Signal a,
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
