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

{-
--MONAD
instance Monad Signal where
  return = pure
  (Signal s) >>= f = Signal $ \t -> let newA = s t
                                        sigB = f newA
                                    in renderer sigB t
-}

instance Monad Signal where
  return = pure
  (Signal s) >>= f = Signal f'
    where f' = \t -> func (f (s t)

                          
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

--testFunc :: Time -> Value
testFunc t = sin t

test = Signal testFunc

anotherFunc :: Value -> Value
anotherFunc a = a + 2


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
