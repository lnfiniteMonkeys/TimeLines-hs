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

toFunc :: (Num a, Fractional a, Floating a) => NumExpr a -> (a -> a)
toFunc expr = case expr of
  IdExpr -> \x -> x
  ConstExpr a -> \_ -> a
  UnExpr op a -> \x -> (toUnFunc op) x
  -- BinExprs can't be translated to (a -> a)s

toSignal :: NumExpr Time -> Signal Time
toSignal = Signal . toFunc

t = IdExpr

data NumExpr a = IdExpr
               | ConstExpr a
               | UnExpr UnOp (NumExpr a)
               | BinExpr BinOp (NumExpr a) (NumExpr a)
  deriving Show

isConst :: NumExpr a -> Bool
isConst (ConstExpr _) = True
isConst _ = False

simplify :: (Num a, Eq a) => NumExpr a -> NumExpr a
simplify e = case e of
  IdExpr -> IdExpr
  ConstExpr a -> ConstExpr a
  UnExpr op exp -> UnExpr op $ simplify exp
  BinExpr op a b -> case op of
    Add -> simplifyAdd e
    Mult -> simplifyMult e
    otherwise -> simplify $ BinExpr op (simplify a) (simplify b)
      
simplifyMult :: (Num a, Eq a) => NumExpr a -> NumExpr a
simplifyMult e = case e of
  BinExpr Mult (ConstExpr 0) _ -> ConstExpr 0
  BinExpr Mult _ (ConstExpr 0) -> ConstExpr 0
  BinExpr Mult (ConstExpr 1) b -> simplify b
  BinExpr Mult a (ConstExpr 1) -> simplify a
  BinExpr Mult (ConstExpr a) (ConstExpr b) -> ConstExpr $ a * b
  BinExpr Mult a b -> simplify $ BinExpr Mult (simplify a) (simplify b)

simplifyAdd :: (Num a, Eq a) => NumExpr a -> NumExpr a
simplifyAdd e = case e of
  BinExpr Add (ConstExpr a) (ConstExpr b) -> ConstExpr $ a + b
  BinExpr Add a b -> simplify $ BinExpr Add (simplify a) (simplify b)

mapTest :: NumExpr Time -> [Value]
mapTest expr =
  let (Signal sf) = toSignal expr
      domain = [0, 0.1..2]
  in map sf domain

-- INSTANCES
instance (Eq a, Num a) => Eq (NumExpr a) where
  e1 == e2 = case (e1, e2) of
    (IdExpr, IdExpr) -> True
    (UnExpr op1 a, UnExpr op2 b) -> simplifiedEq a b
    otherwise -> simplifiedEq e1 e2
 {-   
 (UnExpr op1 a) == (UnExpr op2 b) =
    op1 == op2 && simplifiedEq a b
  e1@(BinExpr Add a b) == e2@(BinExpr Add c d) =
    simplifiedEq e1 e2
    --eitherPairsEqual (a, b) (c, d)
  (BinExpr Mult a b) == (BinExpr Mult c d) =
    eitherPairsEqual (a, b) (c, d)
  (BinExpr Mod a b) == (BinExpr Mod c d) =
    respectivePairsEqual (a, b) (c, d)
  (BinExpr Pow a b) == (BinExpr Pow c d) =
    respectivePairsEqual (a, b) (c, d)
  a == b = simplifiedEq a b
-}

bothAdd (BinExpr Add _ _) (BinExpr Add _ _) = True
bothAdd _ _ = False

bothMult (BinExpr Mult _ _) (BinExpr Mult _ _) = True
bothMult _ _ = False


{- 
  IdExpr == IdExpr = True
  (ConstExpr a) == (ConstExpr b) =
    a == b
  (UnExpr op1 a) == (UnExpr op2 b) =
    op1 == op2 && simplifiedEq a b
  (BinExpr Add a b) == (BinExpr Add c d) =
    eitherPairsEqual (a, b) (c, d)
  (BinExpr Mult a b) == (BinExpr Mult c d) = 
    eitherPairsEqual (a, b) (c, d)
  (BinExpr Pow a b) == (BinExpr Pow c d) =
    respectivePairsEqual (a, b) (c, d)
  (BinExpr op1 a b) == (BinExpr op2 c d) =
    op1 == op2 && eitherPairsEqual (fmap simplify (a, b)) (fmap simplify (c, d))
  _ == _ = False
-}
  
simplifiedEq a b = simplify a == simplify b

isConst0 :: (Num a, Eq a) => NumExpr a -> Bool
isConst0 (ConstExpr 0) = True
isConst0 _ = False

bothConst0 a b = isConst0 a && isConst0 b

-- | (1, 2) = (2, 1)
eitherPairsEqual :: (Num a, Eq a) => (NumExpr a, NumExpr a) -> (NumExpr a, NumExpr a) -> Bool
eitherPairsEqual (a, b) (c, d) = pairs1 || pairs2
  where pairs1 = simplifiedEq a c && simplifiedEq b d
        pairs2 = simplifiedEq a d && simplifiedEq b c

-- | (1, 2) != (2, 1)
respectivePairsEqual :: (Num a, Eq a) => (NumExpr a, NumExpr a) -> (NumExpr a, NumExpr a) -> Bool
respectivePairsEqual (a, b) (c, d) =  simplifiedEq a c && simplifiedEq b d

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
