module EvalTerm(eval) where

import Term
import BuildTerm
import FoldTerm

eval :: Term -> TermB s Term 
eval = foldTermB \case
  TVar x        -> term (TVar x)
  TBool b       -> term (TBool b)
  TInt n        -> term (TInt n)
  TRat r        -> term (TRat r)
  TOp1 op (_,t) -> evalOp1 op t
  TOp2 op (_,t1) (_,t2) -> evalOp2 op t1 t2
  TITE (_,t1) (_,t2) (_,t3) -> evalITE t1 t2 t3

evalITE :: Term -> Term -> Term -> TermB s Term
evalITE cond t2 t3 =
  case termF cond of
    TBool b             -> pure $! if b then t2 else t3
    _ | t2 == t3        -> pure t2
    _                   -> term (TITE cond t2 t3)

evalOp1 :: Op1 -> Term -> TermB s Term
evalOp1 op te =
  case (op, termF te) of
    (Not, TBool x) -> term (TBool (not x))
    (Neg, TInt n)  -> term (TInt (- n))
    _              -> term (TOp1 op te)

evalOp2 :: Op2 -> Term -> Term -> TermB s Term
evalOp2 op t1 t2 =
  case (op, termF t1, termF t2) of

    -- evaluate
    (Add, TInt x, TInt y)          -> term (TInt (x + y))
    (Sub, TInt x, TInt y)          -> term (TInt (x - y))
    (Mul, TInt x, TInt y)          -> term (TInt (x * y))
    (Div, TInt x, TInt y) | y /= 0 -> term (TInt (x `div` y))
    (Mod, TInt x, TInt y) | y /= 0 -> term (TInt (x `mod` y))

    (Add, TRat x, TRat y)          -> term (TRat (x + y))
    (Sub, TRat x, TRat y)          -> term (TRat (x - y))
    (Mul, TRat x, TRat y)          -> term (TRat (x * y))
    (Div, TRat x, TRat y) | y /= 0 -> term (TRat (x / y))
    
    (Eq,  TInt x, TInt y)          -> term (TBool (x == y))
    (Leq, TInt x, TInt y)          -> term (TBool (x <= y))
    (Lt,  TInt x, TInt y)          -> term (TBool (x <  y))

    (Eq,  TRat x, TRat y)          -> term (TBool (x == y))
    (Leq, TRat x, TRat y)          -> term (TBool (x <= y))
    (Lt,  TRat x, TRat y)          -> term (TBool (x <  y))

    (And, TBool x, TBool y)        -> term (TBool (x && y))
    (Or,  TBool x, TBool y)        -> term (TBool (x || y))

    -- units; assumes normalized, see below
    (Add, _, TInt 0)               -> pure t1
    (Mul, _, TInt 1)               -> pure t1
    (Div, _, TInt 1)               -> pure t1

    (Add, _, TRat 0)               -> pure t1
    (Mul, _, TRat 1)               -> pure t1
    (Div, _, TRat 1)               -> pure t1
    
    (And, _, TBool True)           -> pure t1
    (Or,  _, TBool False)          -> pure t1

    -- cancellation
    (Mul, _, TInt 0)               -> term (TInt 0)
    (Mod, _, TInt 1)               -> term (TInt 0)

    (Mul, _, TRat 0)               -> term (TRat 0)

    (And, _, TBool False)          -> term (TBool False)
    (Or,  _, TBool True)           -> term (TBool True)
    (Eq,  _, _) | t1 == t2         -> term (TBool True)

    -- commute
    (Add, TInt {}, _)              -> evalOp2 op t2 t1
    (Mul, TInt {}, _)              -> evalOp2 op t2 t1
    (And, TBool {}, _)             -> evalOp2 op t2 t1
    (Or,  TBool {}, _)             -> evalOp2 op t2 t1
    -- Eq?

    -- assoc
    (Add, _, TOp2 Add x y)        -> evalOp2 op t1 x >>= \xx -> evalOp2 op xx y
    (Mul, _, TOp2 Mul x y)        -> evalOp2 op t1 x >>= \xx -> evalOp2 op xx y
    (And, _, TOp2 And x y)        -> evalOp2 op t1 x >>= \xx -> evalOp2 op xx y
    (Or,  _, TOp2 Or  x y)        -> evalOp2 op t1 x >>= \xx -> evalOp2 op xx y

    -- symbolic
    (_, _, _)                     -> term (TOp2 op t1 t2)



