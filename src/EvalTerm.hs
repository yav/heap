module EvalTerm(evalBuild, Builder) where

import PrimTerm

type Builder m = TermF Term -> m Term 

evalBuild :: Monad m => Builder m -> Builder m
evalBuild doBuild = \sub ->  
  case sub of
    TVar {}           -> doBuild sub
    TBool {}          -> doBuild sub
    TInt {}           -> doBuild sub
    TRat {}           -> doBuild sub
    TOp1 op t         -> evalOp1 doBuild op t
    TOp2 op t1 t2     -> evalOp2 doBuild op t1 t2
    TOp3 op t1 t2 t3  -> evalOp3 doBuild op t1 t2 t3
{-# inline evalBuild #-}


evalOp3 :: Monad m => Builder m -> Op3 -> Term -> Term -> Term -> m Term
evalOp3 doBuild op t1 t2 t3 =
  case op of
    ITE ->
      case termF t1 of
        TBool b             -> pure $! if b then t2 else t3
        _ | t2 == t3        -> pure t2
        _                   -> doBuild (TOp3 ITE t1 t2 t3)
    ArraySet -> doBuild (TOp3 ArraySet t1 t2 t3)
{-# inline evalOp3 #-}


evalOp1 :: Monad m => Builder m -> Op1 -> Term -> m Term
evalOp1 doBuild op te =
  case (op, termF te) of
    (Not, TBool x) -> doBuild (TBool (not x))
    (Neg, TInt n)  -> doBuild (TInt (- n))
    -- -(x - y) ~> y - x
    -- -(x * y) ~> x * (-y)
    _              -> doBuild (TOp1 op te)
{-# inline evalOp1 #-}


evalOp2 :: Monad m => Builder m -> Op2 -> Term -> Term -> m Term
evalOp2 doBuild = go
  where
  go op t1 t2 =
    case (op, termF t1, termF t2) of
  
      -- evaluate
      (Add, TInt x, TInt y)          -> doBuild (TInt (x + y))
      (Sub, TInt x, TInt y)          -> doBuild (TInt (x - y))
      (Mul, TInt x, TInt y)          -> doBuild (TInt (x * y))
      (Div, TInt x, TInt y) | y /= 0 -> doBuild (TInt (x `div` y))
      (Mod, TInt x, TInt y) | y /= 0 -> doBuild (TInt (x `mod` y))
  
      (Add, TRat x, TRat y)          -> doBuild (TRat (x + y))
      (Sub, TRat x, TRat y)          -> doBuild (TRat (x - y))
      (Mul, TRat x, TRat y)          -> doBuild (TRat (x * y))
      (Div, TRat x, TRat y) | y /= 0 -> doBuild (TRat (x / y))
      
      (Eq,  TInt x, TInt y)          -> doBuild (TBool (x == y))
      (Leq, TInt x, TInt y)          -> doBuild (TBool (x <= y))
      (Lt,  TInt x, TInt y)          -> doBuild (TBool (x <  y))
  
      (Eq,  TRat x, TRat y)          -> doBuild (TBool (x == y))
      (Leq, TRat x, TRat y)          -> doBuild (TBool (x <= y))
      (Lt,  TRat x, TRat y)          -> doBuild (TBool (x <  y))
  
      (And, TBool x, TBool y)        -> doBuild (TBool (x && y))
      (Or,  TBool x, TBool y)        -> doBuild (TBool (x || y))
  
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
      (Mul, _, TInt 0)               -> doBuild (TInt 0)
      (Mod, _, TInt 1)               -> doBuild (TInt 0)
  
      (Mul, _, TRat 0)               -> doBuild (TRat 0)
  
      (And, _, TBool False)          -> doBuild (TBool False)
      (Or,  _, TBool True)           -> doBuild (TBool True)
      (Eq,  _, _) | t1 == t2         -> doBuild (TBool True)
  
      -- commute
      (Add, TInt {}, _)              -> go op t2 t1
      (Add, TRat {}, _)              -> go op t2 t1
      (Mul, TInt {}, _)              -> go op t2 t1
      (Mul, TRat {}, _)              -> go op t2 t1
      (And, TBool {}, _)             -> go op t2 t1
      (Or,  TBool {}, _)             -> go op t2 t1
      -- Eq?
  
      -- assoc
      (Add, _, TOp2 Add x y)        -> go op t1 x >>= \xx -> go op xx y
      (Mul, _, TOp2 Mul x y)        -> go op t1 x >>= \xx -> go op xx y
      (And, _, TOp2 And x y)        -> go op t1 x >>= \xx -> go op xx y
      (Or,  _, TOp2 Or  x y)        -> go op t1 x >>= \xx -> go op xx y

      -- assoc eval
      -- (x + Y) + T2 ~> x + (Y + T2)
      (Add, TOp2 op1 x y, TInt {})
        | TInt {} <- termF y, op == op1 && (op == Add || op == Mul)
        -> go op y t2 >>= \xx -> go op x xx
      (Add, TOp2 op1 x y, TRat {})
        | TRat {} <- termF y, op == op1 && (op == Add || op == Mul)
        -> go op y t2 >>= \xx -> go op x xx

      -- XXX: multi op
      -- (x + K1) - K2 ~> x + (K1 - K2)
      -- What's a good way to avoid creating all intermediate terms?
      -- (e.g., 1 + (2 + 3) ~> 1 + 5 ~> 6  (don't need a 5 term)


      -- symbolic
      (_, _, _)                     -> doBuild (TOp2 op t1 t2)
{-# inline evalOp2 #-}  


