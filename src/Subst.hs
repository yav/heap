module Subst
  ( Subst, substFromList, apSubst
  , renameVar, renameVar'
  ) where

import Data.Maybe(fromMaybe)
import Data.IntMap(IntMap)
import Data.IntMap qualified as IntMap
import Term
import BuildTerm
import FoldTerm

newtype Subst = Subst (IntMap Term)

-- | Replace the given variables with the given terms.
substFromList :: [(TVarName,Term)] -> Subst
substFromList xs = Subst (IntMap.fromList [ (tvarId x, t) | (x, t) <- xs ])

-- | Apply a substitution to a term
apSubst :: Subst -> Term -> TermB s Term
apSubst su t = withUser noCache (apSubst' su t)

-- | Apply a substitution to a term, and reuse the existing cache.
-- This is useful if we want to apply the same substitution multiple times.
apSubst' :: Subst -> Term -> TermB (Cache Term, s) Term
apSubst' (Subst su) = substWith \x -> pure $! IntMap.lookup (tvarId x) su

-- | Replace a variable with another variable
renameVar :: TVarName -> TVarName -> Term -> TermB s Term
renameVar x xt t = withUser noCache (renameVar' x xt t)

-- | Replace a varibale with another variable, and reuse the existing cache.
-- This is useful if we want to do the same replacement multiple times.
renameVar' :: TVarName -> TVarName -> Term -> TermB (Cache Term, s) Term
renameVar' y yt = substWith \x -> if x == y then Just <$> term (TVar yt) else pure Nothing

substWith :: (TVarName -> TermB s (Maybe Term)) -> Term -> TermB (Cache Term, s) Term
substWith su = foldTermB' \self selfF ->
  case selfF of
    TVar x          -> fromMaybe self <$> su x
    TBool {}        -> pure self
    TInt {}         -> pure self
    TRat {}         -> pure self
    TOp1 op t       -> term (TOp1 op t)
    TOp2 op t1 t2   -> term (TOp2 op t1 t2)
    TOp3 op t1 t2 t3-> term (TOp3 op t1 t2 t3)
{-# inline substWith #-}