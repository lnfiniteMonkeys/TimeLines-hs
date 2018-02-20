module Signal
  ( Signal(..)
    ,Time
    ,Value
    ,sig    
  ) where

import Data.Typeable
import Control.Applicative
import Prelude

--Time and value are the inputs and outputs of a Signal
type Time = Rational
type Value = Double

--A signal of value type a gets constructed by passing a function from time to a
data Signal a = Signal {valueAt:: Time -> a}
  deriving Typeable

---------------INSTANCES---------------
--FUNCTOR
instance Functor Signal where
--  fmap f (Signal a) = Signal $ fmap f a
  fmap f sig = Signal $ f . valueAt sig
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
                                    in valueAt sigB t
  
instance (Fractional a, Eq a) => Fractional (Signal a) where
  recip        = fmap recip
  fromRational = pure . fromRational


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

--NUM
instance (Num a, Eq a) => Num (Signal a) where
      negate      = fmap negate
      (+)         = liftA2 (+)
      (*)         = liftA2 (.*) -- lazy multiplicator
      fromInteger = pure . fromInteger
      abs         = fmap abs
      signum      = fmap signum


(.*) :: (Num a, Eq a) => a -> a -> a
(.*) 0 _ = 0
(.*) a b = a * b



sig :: (Time -> a) -> Signal a   
sig f = Signal $ \t -> f t

--Slows time for a signal by a factor
slow :: Signal a -> Time -> Signal a
slow (Signal f) fact = Signal $ \t -> let t' = t/fact 
                                      in f t'

fast :: Signal a -> Time -> Signal a
fast sig a = slow sig (-a)




constSig :: a -> Signal a
constSig s = Signal $ \_ -> s

--testFunc :: Time -> Value
testFunc t = sin t


anotherFunc :: Value -> Value
anotherFunc a = a + 2




testA :: Signal Double
testA = 0.3


testB :: Rational
testB = 0.2








{-
--Num instance for single-argument functions
instance Num b => Num (a -> b) where
      negate      = fmap negate
      (+)         = liftA2 (+)
      (*)         = liftA2 (*)
      fromInteger = pure . fromInteger
      abs         = fmap abs
      signum      = fmap signum

combo = sin + cos
-}


