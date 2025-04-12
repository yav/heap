module FoldTerm(foldTerm, foldTermB, foldTermB', Cache, noCache) where

import Data.IntMap qualified as IntMap

import Term
import BuildTerm

foldTermB :: (Term -> TermF a -> TermB s a) -> Term -> TermB s a
foldTermB f t0 = withUser noCache (foldTermB' f t0)


newtype Cache a = Cache (IntMap.IntMap a)

noCache :: Cache a
noCache = Cache mempty

foldTermB' :: (Term -> TermF a -> TermB s a) -> Term -> TermB (Cache a, s) a
foldTermB' f t0 = go t0
  where
  go te =
    do let tid = termId te
           doWork x =
             do a <- liftUser (f te x)
                updUser \(Cache cache,user) ->
                  let !c = IntMap.insert tid a cache
                  in (Cache c,user)
                pure a
               
       (Cache cache,_) <- getUser
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
                  doWork (TOp1 op x)
             TOp2 op t1 t2 ->
               do x <- go t1
                  y <- go t2
                  doWork (TOp2 op x y)
             TOp3 op t1 t2 t3 ->
               do x <- go t1
                  y <- go t2
                  z <- go t3
                  doWork (TOp3 op x y z)
{-# INLINE foldTermB #-}

foldTerm :: (Term -> TermF a -> a) -> Term -> a
foldTerm f = build () . foldTermB (\t tf -> pure (f t tf))
{-# INLINE foldTerm #-}