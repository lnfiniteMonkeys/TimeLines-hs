module Signal
  (  Signal(..)
    ,Time
    ,Value
  ) where

import Control.Applicative
import Prelude

type Value = Double
type Time = Double

newtype Signal = Signal {sigFunc :: Time -> Value}

-- don't evaluate second term if first term is 0
(*.) :: (Num a, Eq a) => a -> a -> a
(*.) 0 _ = 0
(*.) a b = a * b

--shortcut to use with $ for RHS argument
mu = (*.)

