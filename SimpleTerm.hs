module SimpleTerm where

newtype TVarName = TVarName Int
  deriving Eq

data Op1  = Neg | Not
  deriving Eq

data Op2  = Add | Sub | Mul | Div | Mod
          | Eq | Leq | Lt
          | And | Or
  deriving Eq

data TermF term =
    TVar !TVarName
  | TBool !Bool
  | TInt !Integer
  | TOp1 !Op1 !term
  | TOp2 !Op2 !term !term
  | TITE !term !term !term
    deriving (Eq, Functor, Foldable, Traversable)

newtype Term = Term (TermF Term)
  deriving Eq

var :: Int -> Term
var n = Term (TVar (TVarName n))

true :: Term
true = Term (TBool True)

false :: Term
false = Term (TBool False)

tnot :: Term -> Term
tnot x = Term (TOp1 Not x)

ite :: Term -> Term -> Term -> Term
ite x y z = Term (TITE x y z)

instance Num Term where
  x + y         = Term (TOp2 Add x y)
  x - y         = Term (TOp2 Sub x y)
  x * y         = Term (TOp2 Mul x y)
  negate x      = Term (TOp1 Neg x)
  abs           = error "Term: `abs` is not available."
  signum        = error "Term: `signum` is not available."
  fromInteger x = Term (TInt x)

(./) :: Term -> Term -> Term
x ./ y = Term (TOp2 Div x y)

(.%) :: Term -> Term -> Term
x .% y = Term (TOp2 Mod x y)

(.==) :: Term -> Term -> Term
x .== y = Term (TOp2 Eq x y)

(.<=) :: Term -> Term -> Term
x .<= y = Term (TOp2 Leq x y)

(.<) :: Term -> Term -> Term
x .< y = Term (TOp2 Lt x y)

(.&&) :: Term -> Term -> Term
x .&& y = Term (TOp2 And x y)

(.||) :: Term -> Term -> Term
x .|| y = Term (TOp2 Or x y)


