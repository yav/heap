module FoldTerm(foldTerm, foldTermB) where

import Data.IntMap qualified as IntMap

import Term
import BuildTerm

foldTermB :: (TermF (Term,a) -> TermB s a) -> Term -> TermB s a
foldTermB f t0 = withUser mempty (go t0)
  where
  go te =
    do let tid = termId te
           doWork x =
             do a <- liftUser (f x)
                updUser \(cache,user) ->
                  let !c = IntMap.insert tid a cache
                  in (c,user)
                pure a
               
       (cache,_) <- getUser
       case IntMap.lookup tid cache of
         Just a -> pure a
         Nothing ->
           case termF te of
             TVar x -> doWork (TVar x)
             TBool b -> doWork (TBool b)
             TInt n -> doWork (TInt n)
             TRat r -> doWork (TRat r)
             TOp1 op t ->
               do x <- go t
                  doWork (TOp1 op (t,x))
             TOp2 op t1 t2 ->
               do x <- go t1
                  y <- go t2
                  doWork (TOp2 op (t1,x) (t2,y))
             TITE t1 t2 t3 ->
               do x <- go t1
                  y <- go t2
                  z <- go t3
                  doWork (TITE (t1,x) (t2,y) (t3,z))
{-# INLINE foldTermB #-}

foldTerm :: (TermF (Term,a) -> a) -> Term -> a
foldTerm f = build () . foldTermB (pure . f)
{-# INLINE foldTerm #-}