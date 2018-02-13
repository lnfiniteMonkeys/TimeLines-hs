module Signal
  ( Signal(..),
    sig    
  ) where


import Data.Typeable
import Control.Applicative

type Time = Double
type Value = Double


--Function from time to value
data Signal a = Signal {renderer:: Time -> a}
  deriving Typeable

--FUNCTOR
instance Functor Signal where
  fmap f (Signal a) = Signal $ fmap f a
  (<$) = fmap . const

--APPLICATIVE
instance Applicative Signal where
  pure a = constSig a -- Transform a value "a" to a signal of constant value "a"
  -- A signal "f" of type "Time -> (a -> b)", applied at Time "t" to a signal "x"
  -- is equal to the value of f for Time t, applied to the value of x for Time t
  (Signal f) <*> (Signal x) = Signal $ \t -> f t $ x t 

--NUM
instance Num a => Num (Signal a) where
      negate      = fmap negate
      (+)         = liftA2 (+)
      (*)         = liftA2 (*)
      fromInteger = pure . fromInteger
      abs         = fmap abs
      signum      = fmap signum

sig :: (Time -> a) -> Signal a 
sig f = Signal $ \t -> f t


constSig :: a -> Signal a
constSig s = Signal $ \_ -> s




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

