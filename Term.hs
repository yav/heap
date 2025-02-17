{-# Language ImportQualifiedPost #-}
{-# Language BangPatterns #-}
module Term
  ( Term(..)
  , Context, tapp
  , Subst, apSubst
  ) where

import Data.Maybe(fromMaybe)
import Data.Map(Map)
import Data.Map qualified as Map

data Term =
    TVar {-# UNPACK #-} !Int
  | TCon {-# UNPACK #-} !Int
  | TApp !Term !Term !Int

instance Eq Term where
  t1 == t2 =
    case (t1,t2) of
      (TVar x, TVar y)          -> x == y
      (TCon x, TCon y)          -> x == y
      (TApp _ _ x, TApp _ _ y)  -> x == y
      _                         -> False

instance Ord Term where
  compare t1 t2 =
    case (t1,t2) of
      (TVar x, TVar y) -> compare x y
      (TVar _, _)      -> LT
      (_, TVar _)      -> GT

      (TCon x, TCon y) -> compare x y
      (TCon _, _)      -> LT
      (_, TCon _)      -> GT

      (TApp _ _ x, TApp _ _ y) -> compare x y

data Context = Context
  { apps :: Map (Term,Term) Term
  , next :: {-# UNPACK #-} !Int
  }

tapp :: Term -> Term -> Context -> (Term,Context)
tapp t1 t2 ctxt =
  case Map.lookup (t1,t2) (apps ctxt) of
    Just t -> (t, ctxt)
    Nothing ->
      let !n = next ctxt
          !t = TApp t1 t2 n
          !c = Context { apps = Map.insert (t1,t2) t (apps ctxt), next = n + 1 }
      in (t,c)

type Subst = Map Int Term
type Cache = Map Int (Maybe Term)

apSubstMaybe :: Subst -> Term -> Context -> Cache -> (Maybe (Term, Context), Cache)
apSubstMaybe su t ctxt cache =
  case t of
    TVar n ->
      ( do t1 <- Map.lookup n su
           pure (t1,ctxt)
      , cache
      )

    TCon _ -> (Nothing, cache)

    TApp t1 t2 n ->
      case Map.lookup n cache of
        Just mb ->
          ( do t1' <- mb
               pure (t1',ctxt)
          , cache
          )
        Nothing ->
          let (mb1, cache1) = apSubstMaybe su t1 ctxt cache
          in case mb1 of
               Nothing ->
                 let (mb2, cache2) = apSubstMaybe su t2 ctxt cache1
                 in case mb2 of
                      Nothing ->
                        let !cache3 = Map.insert n Nothing cache2
                        in (Nothing, cache3)

                      Just (t3,ctxt1) ->
                        let (!t4, !ctxt2) = tapp t1 t3 ctxt1
                            !cache3 = Map.insert n (Just t4) cache2
                        in (Just (t4, ctxt2), cache3)

               Just (t1',ctxt1) ->
                 let (mb2, cache2)  = apSubstMaybe su t2 ctxt1 cache1
                     (t2',ctxt2)    = fromMaybe (t2,ctxt1) mb2
                     (!t3,!ctxt3)   = tapp t1' t2' ctxt2
                     !cache3        = Map.insert n (Just t3) cache2
                 in (Just (t3,ctxt3), cache3)


apSubst :: Map Int Term -> Term -> Context -> (Term, Context)
apSubst su t ctxt =
  case fst (apSubstMaybe su t ctxt mempty) of
    Just (t1,ctxt1) -> (t1,ctxt1)
    Nothing         -> (t,ctxt)
{-# INLINE apSubst #-}



