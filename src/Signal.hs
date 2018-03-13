module Signal
  (  Signal(..)
    ,Time
    ,Value    
  ) where

import Data.Typeable
import Control.Applicative
import Prelude

--Time and value are the inputs and outputs of a Signal
type Time = Double
type Value = Double

--A signal of value type a gets constructed by passing a function from time to a
data Signal a = Signal {sigFunc :: Time -> a}
  deriving Typeable


---------------INSTANCES---------------
--FUNCTOR
instance Functor Signal where
--  fmap f (Signal a) = Signal $ fmap f a
  fmap f sig = Signal $ f . sigFunc sig
  (<$) = fmap . const

--APPLICATIVE
instance Applicative Signal where
  -- Transform a value to a signal of constant value
  pure = constSig
  -- A signal with function "f" of type "Time -> (a -> b)", applied at Time "t" to
  -- a signal with function "x" of type "Time -> b", is equal to the value of f for
  -- Time t, applied to the value of x for Time t
  (Signal f) <*> (Signal x) = Signal $ \t -> f t $ x t
  --f <*> x = Signal $ \t -> valueAt f t $ valueAt x t

--MONAD
instance Monad Signal where
  return = pure
  -- :: Signal a -> (a -> Signal b) -> Signal b
  (Signal s) >>= f = Signal $ \t -> let newA = s t
                                        sigB = f newA
                                    in  sigFunc sigB t
                                        
--FRACTIONAL
instance (Fractional a, Eq a) => Fractional (Signal a) where
  recip        = fmap recip
  fromRational = pure . fromRational


--instance (RealFrac a) => RealFrac (Signal a) where
--floor :: (RealFrac a) => Signal a -> Signal a
--floor (Signal a) = Signal $ \t -> fromIntegral . Prelude.floor $ a t

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
  acosh = fmap acos

-- don't evaluate second term if first term is 0
(.*) :: (Num a, Eq a) => a -> a -> a
(.*) 0 _ = 5
(.*) a b = a * b

--NUM
instance (Num a, Eq a) => Num (Signal a) where
      negate      = fmap negate
      (+)         = liftA2 (+)
      (*)         = liftA2 (.*) -- lazy multiplicator
      fromInteger = pure . fromInteger
      abs         = fmap abs
      signum      = fmap signum

--Num instance for single-argument functions
instance (Num a, Num b, Eq a, Eq b) => Num (a -> b) where
      negate      = fmap negate
      (+)         = liftA2 (+)
      (*)         = liftA2 (.*)
      fromInteger = pure . fromInteger
      abs         = fmap abs
      signum      = fmap signum


--Speeds up time for a signal by a factor
fast :: Signal a -> Signal Time -> Signal a
fast (Signal sf) (Signal r) = Signal $ \t -> let t' = t * (r t)
                                             in  sf t'
slow :: Signal a -> Signal Time -> Signal a
slow s r = fast s (1/r) 

constSig :: a -> Signal a
constSig s = Signal $ \_ -> s


