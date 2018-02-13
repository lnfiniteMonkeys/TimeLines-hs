module Signal
  ( Signal,
    sig    
  ) where

import Data.Typeable
import Control.Applicative

type Time = Double
type Value = Double


--A signal of value a gets constructed by passing a function from time to a
data Signal a = Signal {renderer:: Time -> a}
  deriving Typeable


---------------INSTANCES---------------
--FUNCTOR
instance Functor Signal where
  fmap f (Signal a) = Signal $ fmap f a
  (<$) = fmap . const

--APPLICATIVE
instance Applicative Signal where
  -- Transform a value "a" to a signal of constant value "a"
  pure = constSig
  -- A signal with function "f" of type "Time -> (a -> b)", applied at Time "t" to
  -- a signal with function "x" of type "Time -> b", is equal to the value of f for
  -- Time t, applied to the value of x for Time t
  (Signal f) <*> (Signal x) = Signal $ \t -> f t $ x t 


--MONAD
instance Monad Signal where
  return = pure
  (Signal s) >>= f = Signal $ \t -> let newA = s t
                                        sigB = f newA
                                    in renderer sigB t

--Enum
instance Enum a => Enum (Signal a) where
  succ   = fmap succ
  pred   = fmap pred
  toEnum = pure . toEnum
  fromEnum       = pure . fromEnum
  enumFrom       = pure . enumFrom
  enumFromThen   = noOv "enumFromThen"
  enumFromTo     = noOv "enumFromTo"
  enumFromThenTo = noOv "enumFromThenTo"

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
  acosh = fmap acosh







{-
instance Monad Signal where
  return = pure
  (Signal s) >>= f = Signal f'
    where f' = \t -> renderer (f (s t))
-}




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

constSig :: a -> Signal a
constSig s = Signal $ \_ -> s

--testFunc :: Time -> Value
testFunc t = sin t

test = Signal testFunc

anotherFunc :: Value -> Value
anotherFunc a = a + 2




testA :: Signal Double
testA = 0.34










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


