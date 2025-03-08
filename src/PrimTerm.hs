module PrimTerm where

data TVarName = TVarName Int Type

instance Eq TVarName where
  TVarName x _ == TVarName y _ = x == y

instance Ord TVarName where
  compare (TVarName x _) (TVarName y _) = compare x y

data TFunName = TFunName Int Type -- ^ Type of result 

instance Eq TFunName where
  TFunName x _ == TFunName y _ = x == y

instance Ord TFunName where
  compare (TFunName x _) (TFunName y _) = compare x y



data Type = Boolean | Integer | Rational | Location
  deriving (Eq,Ord)

data Op1  = Neg | Not | App1 TFunName
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
    deriving (Eq,Ord,Functor)
  
data Term = Term
  { termId :: !Int
  , termF  :: TermF Term
  }

instance Eq Term where
  x == y = termId x == termId y

instance Ord Term where
  compare x y = compare (termId x) (termId y)

class TypeOf t where
  typeOf :: t -> Type

instance TypeOf TVarName where
  typeOf (TVarName _ t) = t

instance TypeOf TFunName where
  typeOf (TFunName _ t) = t

instance TypeOf term => TypeOf (TermF term) where
  typeOf t =
    case t of
      TVar x   -> typeOf x
      TBool {} -> Boolean
      TInt {}  -> Integer
      TRat {}  -> Rational

      TOp1 op t1 ->
        case op of
          App1 f -> typeOf f
          Not    -> Boolean
          Neg    -> typeOf t1

      TOp2 op t1 _ ->
        case op of
          Add -> typeOf t1
          Sub -> typeOf t1
          Mul -> typeOf t1
          Div -> typeOf t1
          Mod -> typeOf t1
          Eq  -> Boolean
          Leq -> Boolean
          Lt  -> Boolean
          And -> Boolean
          Or  -> Boolean

      TITE _ t1 _ -> typeOf t1
    
