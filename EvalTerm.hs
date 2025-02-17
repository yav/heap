{-# Language BlockArguments, LambdaCase #-}
module EvalTerm where

import SimpleTerm
import FoldTerm

eval :: Term -> Term
eval = fold \case
  TVar x        -> Term (TVar x)
  TBool b       -> Term (TBool b)
  TInt n        -> Term (TInt n)
  TOp1 op t     -> evalOp1 op t
  TOp2 op t1 t2 -> evalOp2 op t1 t2
  TITE t1 t2 t3 -> evalITE t1 t2 t3

evalITE :: Term -> Term -> Term -> Term
evalITE t1@(Term cond) t2 t3 =
  case cond of
    TBool b             -> if b then t2 else t3
    _ | t2 == t3        -> t2
    _                   -> Term (TITE t1 t2 t3)

evalOp1 :: Op1 -> Term -> Term
evalOp1 op (Term term) =
  case (op, term) of
    (Not, TBool x) -> Term (TBool (not x))
    (Neg, TInt n)  -> Term (TInt (- n))
    (_, t1)        -> Term (TOp1 op (Term t1))

evalOp2 :: Op2 -> Term -> Term -> Term
evalOp2 op t1@(Term term1) t2@(Term term2) =
  case (op, term1, term2) of

    -- evaluate
    (Add, TInt x, TInt y)          -> Term (TInt (x + y))
    (Sub, TInt x, TInt y)          -> Term (TInt (x - y))
    (Mul, TInt x, TInt y)          -> Term (TInt (x * y))
    (Div, TInt x, TInt y) | y /= 0 -> Term (TInt (x `div` y))
    (Mod, TInt x, TInt y) | y /= 0 -> Term (TInt (x `mod` y))

    (Eq,  TInt x, TInt y)          -> Term (TBool (x == y))
    (Leq, TInt x, TInt y)          -> Term (TBool (x <= y))
    (Lt,  TInt x, TInt y)          -> Term (TBool (x <  y))

    (And, TBool x, TBool y)        -> Term (TBool (x && y))
    (Or,  TBool x, TBool y)        -> Term (TBool (x || y))

    -- units; assumes normalized, see below
    (Add, _, TInt 0)               -> t1
    (Mul, _, TInt 1)               -> t1
    (Div, _, TInt 1)               -> t1
    (And, _, TBool True)           -> t1
    (Or,  _, TBool False)          -> t1

    -- cancellation
    (Mul, _, TInt 0)              -> Term (TInt 0)
    (Mod, _, TInt 1)              -> Term (TInt 0)
    (And, _, TBool False)         -> Term (TBool False)
    (Or,  _, TBool True)          -> Term (TBool True)
    (Eq,  _, _) | term1 == term2  -> Term (TBool True)

    -- commute
    (Add, TInt {}, _)             -> evalOp2 op t2 t1
    (Mul, TInt {}, _)             -> evalOp2 op t2 t1
    (And, TBool {}, _)            -> evalOp2 op t2 t1
    (Or,  TBool {}, _)            -> evalOp2 op t2 t1
    -- Eq?

    -- assoc
    (Add, _, TOp2 Add x y)        -> evalOp2 op (evalOp2 op t1 x) y
    (Mul, _, TOp2 Mul x y)        -> evalOp2 op (evalOp2 op t1 x) y
    (And, _, TOp2 And x y)        -> evalOp2 op (evalOp2 op t1 x) y
    (Or,  _, TOp2 Or  x y)        -> evalOp2 op (evalOp2 op t1 x) y

    -- symbolic
    (_, _, _)                     -> Term (TOp2 op t1 t2)



