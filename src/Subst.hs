module Subst
  ( Subst, substFromList, apSubst
  ) where

import Data.IntMap(IntMap)
import Data.IntMap qualified as IntMap
import Term
import BuildTerm
import FoldTerm

newtype Subst = Subst (IntMap Term)

substFromList :: [(TVarName,Term)] -> Subst
substFromList xs = Subst (IntMap.fromList [ (n,t) | (TVarName n, t) <- xs ])

apSubst :: Subst -> Term -> TermB s Term
apSubst (Subst su) = foldTermB \self selfF ->
  case selfF of
    TVar (TVarName x) ->
      case IntMap.lookup x su of
        Just t -> pure t
        Nothing -> pure self
    TBool {}        -> pure self
    TInt {}         -> pure self
    TRat {}         -> pure self
    TOp1 op t       -> term (TOp1 op t)
    TOp2 op t1 t2   -> term (TOp2 op t1 t2)
    TITE c t1 t2    -> term (TITE c t1 t2)