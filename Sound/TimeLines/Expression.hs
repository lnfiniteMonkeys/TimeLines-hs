module Sound.TimeLines.Expression where

import Data.Fixed
import Sound.TimeLines.Types

data UnOp = Negate | Inverse | Exp | Log | Sin | Abs 
          | Signum | Sqrt | Cos | Asin | Atan | Acos 
          | Sinh | Cosh | Asinh | Atanh | Acosh 
  deriving (Eq, Show)


data BinOp = Add | Mult | Pow | Mod
  deriving (Eq, Show)


toUnFunc :: (Num a, Floating a, Fractional a) => UnOp -> (a -> a)
toUnFunc op = case op of
  Negate -> negate
  Inverse -> \x -> 1 / x
  Exp -> exp
  Log -> log
  Sin -> sin
  Abs -> abs
  Signum -> signum
  Sqrt -> sqrt
  Cos -> cos
  Asin -> asin
  Atan -> atan
  Acos -> acos
  Sinh -> sinh
  Cosh -> cosh
  Asinh -> asinh
  Atanh -> atanh
  Acosh -> acosh

toBinFunc :: (Num a, Floating a, Real a) => BinOp -> (a -> a -> a)
toBinFunc op = case op of
  Add -> (+)
  Mult -> (*)
  Pow -> (**)
  Mod -> mod'
  
data NumExpr a = IdExpr
               | ConstExpr a
               | UnExpr UnOp (NumExpr a)
               | BinExpr BinOp (NumExpr a) (NumExpr a)
  deriving Show

toFunc :: (Num a, Fractional a, Floating a) => NumExpr a -> (a -> a)
toFunc expr = case expr of
  IdExpr -> \x -> x
  ConstExpr a -> \_ -> a
  UnExpr op a -> \x -> (toUnFunc op) x
  -- possible?  BinExpr op a b -> \x y -> (toBinOp)x y

toSignal :: NumExpr Time -> Signal Time
toSignal = Signal . toFunc

t = IdExpr

mapTest :: NumExpr Time -> [Value]
mapTest expr =
  let (Signal sf) = toSignal expr
      domain = [0, 0.1..2]
  in map sf domain


-- INSTANCES
instance (Eq a, Num a) => Eq (NumExpr a) where
  IdExpr == IdExpr = True
  (ConstExpr a) == (ConstExpr b) = a == b
  (UnExpr op1 a) == (UnExpr op2 b) = op1 == op2 && a == b
  (BinExpr Add a b) == (BinExpr Add c d) = anyPairEqual (a, b) (c, d)
  (BinExpr Mult a b) == (BinExpr Mult c d)
    | a == 0 && c == 0 = True
    | a == 0 && d == 0 = True
    | b == 0 && c == 0 = True
    | b == 0 && d == 0 = True
    | otherwise = anyPairEqual (a, b) (c, d)
  (BinExpr Pow a b) == (BinExpr Pow c d) = respectivePairsEqual (a, b) (c, d)
  (BinExpr op1 a b) == (BinExpr op2 c d) = op1 == op2 && anyPairEqual (a, b) (c, d)
  _ == _ = False
  
isConst0 :: (Num a, Eq a) => NumExpr a -> Bool
isConst0 (ConstExpr 0) = True
isConst0 _ = False

bothConst0 a b = isConst0 a && isConst0 b

-- | (1, 2) = (2, 1)
anyPairEqual :: Eq a => (a, a) -> (a, a) -> Bool
anyPairEqual (a, b) (c, d) = pairs1 || pairs2
  where pairs1 = a == c && b == d
        pairs2 = a == d && b == c

-- | (1, 2) != (2, 1)
respectivePairsEqual :: Eq a => (a, a) -> (a, a) -> Bool
respectivePairsEqual (a, b) (c, d) = a == c && b == d

instance (Num a, Eq a) => Num (NumExpr a) where
  negate      = UnExpr Negate
  (+)         = BinExpr Add
  (*)         = BinExpr Mult
  fromInteger = ConstExpr . fromInteger
  abs         = UnExpr Abs
  signum      = UnExpr Signum

instance (Fractional a, Eq a) => Fractional (NumExpr a) where
  recip = UnExpr Inverse
  fromRational = ConstExpr . fromRational

instance (Floating a, Eq a) => Floating (NumExpr a) where
  pi    = ConstExpr pi
  sqrt  = UnExpr Sqrt
  exp   = UnExpr Exp
  log   = UnExpr Log
  sin   = UnExpr Sin
  cos   = UnExpr Cos
  asin  = UnExpr Asin
  atan  = UnExpr Atan
  acos  = UnExpr Acos
  sinh  = UnExpr Sinh
  cosh  = UnExpr Cosh
  asinh = UnExpr Asinh
  atanh = UnExpr Atanh
  acosh = UnExpr Acosh
