module FoldTerm where

import SimpleTerm

fold :: (TermF a -> a) -> Term -> a
fold f (Term term) =
  case term of
    TVar x   -> f (TVar x)
    TBool b  -> f (TBool b)
    TInt n   -> f (TInt n)
    TOp1 op t ->
      let a = fold f t
      in f (TOp1 op a)
    TOp2 op t1 t2 ->
      let a = fold f t1
          b = fold f t2
      in f (TOp2 op a b)
    TITE t1 t2 t3 ->
      let a = fold f t1
          b = fold f t2
          c = fold f t3
      in f (TITE a b c)


fold' :: (TermF (Term, a) -> a) -> Term -> a
fold' f (Term term) =
  case term of
    TVar x   -> f (TVar x)
    TBool b  -> f (TBool b)
    TInt n   -> f (TInt n)
    TOp1 op t ->
      let a = fold' f t
      in f (TOp1 op (t, a))
    TOp2 op t1 t2 ->
      let a = fold' f t1
          b = fold' f t2
      in f (TOp2 op (t1, a) (t2, b))
    TITE t1 t2 t3 ->
      let a = fold' f t1
          b = fold' f t2
          c = fold' f t3
      in f (TITE (t1, a) (t2, b) (t3, c))

