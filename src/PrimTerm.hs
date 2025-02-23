module PrimTerm where

newtype TVarName = TVarName Int
  deriving (Eq,Ord)

data Op1  = Neg | Not
  deriving (Eq,Ord)

data Op2  = Add | Sub | Mul | Div | Mod
          | Eq | Leq | Lt
          | And | Or
  deriving (Eq,Ord)


data TermF term =
    TVar !TVarName
  | TBool !Bool
  | TInt !Integer
  | TRat !Rational
  | TOp1 !Op1 !term
  | TOp2 !Op2 !term !term
  | TITE !term !term !term
    deriving (Eq,Ord)
  
data Term = Term
  { termId :: !Int
  , termF  :: TermF Term
  }

instance Eq Term where
  x == y = termId x == termId y

instance Ord Term where
  compare x y = compare (termId x) (termId y)

