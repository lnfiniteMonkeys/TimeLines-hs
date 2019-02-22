module Sound.TimeLines.Expression where

import Data.Fixed
import Sound.TimeLines.Types

data UnOp = Identity | Negate | Inverse | Exp | Log | Sin | Abs 
          | Signum | Sqrt | Cos | Asin | Atan | Acos | Floor
          | Sinh | Cosh | Asinh | Atanh | Acosh | Truncate
  deriving (Eq, Show)

data BinOp = Add | Mult | Pow | Mod | ApplyToTime
  deriving (Eq, Show)

toUnFunc :: UnOp -> (Value -> Value)
toUnFunc op = case op of
  Identity -> id
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
  Floor -> fromIntegral . floor
  Truncate -> fromIntegral . truncate
--  Fract -> 
--truncateExpr :: SigExpr -> SigExpr
--truncateExpr exp = 
  
toBinFunc :: BinOp -> (Value -> Value -> Value)
toBinFunc op = case op of
  Add -> (+)
  Mult -> (*)
  Pow -> (**)
  Mod -> mod'


toFunc :: SigExpr -> (Time -> Value)
toFunc expr = case expr of
  IdExpr -> \x -> x
  ConstExpr a -> \_ -> a
  UnExpr op a -> \x -> (toUnFunc op) $ (toFunc a) x
  BinExpr op a b -> \x -> (toBinFunc op) (toFunc a x) (toFunc b x)
  ApplyArgExpr a b -> \x -> toFunc b $ (toFunc a) x


toSignal :: SigExpr -> Signal Value
toSignal = Signal . toFunc

a = BinExpr Add (UnExpr Negate $ ConstExpr 3) (BinExpr Mult (UnExpr Exp (IdExpr)) (ConstExpr 5))


slow :: SigExpr -> SigExpr -> SigExpr
slow amt e = ApplyArgExpr (BinExpr Mult (1/amt) IdExpr) e

slow2 :: SigExpr -> SigExpr -> SigExpr
slow2 amt IdExpr = amt * time


--t = ArgExpr IdExpr
time = IdExpr

-- ConstExpr v -> \_ -> v
-- IdExpr -> \x -> x 
-- ArgExpr f -> \x -> f x           (ArgExpr IdExpr = IdExpr)
-- UnExpr f expr -> \x -> f $ expr x
-- BinExpr f expr1 expr1 -> \x -> f (expr1 x) (expr2 x)
data SigExpr = IdExpr
             | ConstExpr Value
             | UnExpr UnOp SigExpr
             | BinExpr BinOp SigExpr SigExpr
             | ApplyArgExpr SigExpr SigExpr
  deriving Show

-- ArgExpr (UnExpr Abs IdExpr)
-- TODO
simplify :: SigExpr -> SigExpr
simplify e = case e of
  IdExpr -> IdExpr
  ConstExpr a -> ConstExpr a
  UnExpr op exp -> case exp of
    ConstExpr a -> ConstExpr $ (toUnFunc op) a
  BinExpr op a b -> case op of
    Add -> simplifyAdd (simplify a) (simplify b)
    Mult -> simplifyMult (simplify a) (simplify b)
    otherwise -> simplify $ BinExpr op (simplify a) (simplify b)
  

simplifyMult :: SigExpr -> SigExpr -> SigExpr
simplifyMult a b
  | eitherIsConst0 a b = 0
  | a == 1 = b
  | b == 1 = a
  | bothConst a b  = ConstExpr $ (fromConst a) * (fromConst b)
  | otherwise = a * b

simplifyPow :: SigExpr -> SigExpr -> SigExpr
simplifyPow a b
  | bothConst0 a b = 1
  | a == 0 = 0
  | b == 0 = 1
  | a == 1 = 1
  | b == 1 = a
  | isPow a = BinExpr Pow (powBase a) (simplify $ b * (powTop a))
  | otherwise = a ** b

simplifyAdd :: SigExpr -> SigExpr -> SigExpr
simplifyAdd a b
  | a == 0 = b
  | b == 0 = a
  | bothConst a b = ConstExpr $ (fromConst a) + (fromConst b)
  | otherwise = a + b


powBase (BinExpr Pow a _) = a
powBase _ = undefined

powTop (BinExpr Pow _ b) = b
powTop _ = undefined

isPow (BinExpr Pow _ _) = True
isPow _ = False

bothConst (ConstExpr _) (ConstExpr _) = True
bothConst _ _ = False

fromConst (ConstExpr a) = a
fromConst _ = undefined

eitherIsConst0 a b = isConst0 a || isConst0 b

isConst1 (ConstExpr 1) = True
isConst1 _ = False

mapTest :: SigExpr -> [Value]
mapTest expr =
  let (Signal sf) = toSignal expr
      domain = [0, 0.1..2]
  in map sf domain

isConst :: SigExpr -> Bool
isConst (ConstExpr _) = True
isConst _ = False

instance Eq SigExpr where
  ConstExpr a == ConstExpr b = a == b
  UnExpr op1 a == UnExpr op2 b =
    op1 == op2 && a == b
  BinExpr op1 a b == BinExpr op2 c d =
    op1 == op2 && binExprArgEq op1 (a, b) (c, d)
  _ == _ = False

simplifiedEq a b = simplify a == simplify b

binExprArgEq op (a, b) (c, d) = eqTest (a, b) (c, d)
  where eqTest = if commutativeOp op
          then eitherPairsEqual else respectivePairsEqual
  
commutativeOp :: BinOp -> Bool
commutativeOp op
  | op == Add || op == Mult = True
  | otherwise = False
  
bothAdd (BinExpr Add _ _) (BinExpr Add _ _) = True
bothAdd _ _ = False

bothMult (BinExpr Mult _ _) (BinExpr Mult _ _) = True
bothMult _ _ = False
  
isConst0 :: SigExpr -> Bool
isConst0 (ConstExpr 0) = True
isConst0 _ = False

bothConst0 a b = isConst0 a && isConst0 b

-- | (1, 2) = (2, 1)
eitherPairsEqual :: (SigExpr, SigExpr) -> (SigExpr, SigExpr) -> Bool
eitherPairsEqual (a, b) (c, d) = pairs1 || pairs2
  where pairs1 = simplifiedEq a c && simplifiedEq b d
        pairs2 = simplifiedEq a d && simplifiedEq b c

-- | (1, 2) != (2, 1)
respectivePairsEqual :: (SigExpr, SigExpr) -> (SigExpr, SigExpr) -> Bool
respectivePairsEqual (a, b) (c, d) =  simplifiedEq a c && simplifiedEq b d

instance Num (SigExpr) where
  negate      = UnExpr Negate
  (+)         = BinExpr Add
  (*)         = BinExpr Mult
  fromInteger = ConstExpr . fromInteger
  abs         = UnExpr Abs
  signum      = UnExpr Signum

instance Fractional SigExpr where
  recip = UnExpr Inverse
  fromRational = ConstExpr . fromRational

instance Floating SigExpr where
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

  
