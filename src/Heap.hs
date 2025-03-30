module Heap where

import Control.Monad(liftM,ap)

import Term
import BuildTerm
import Subst

data HeapLoc = HeapEntry {
  locName   :: !TVarName,
  locVal    :: !Term,
  locPerms  :: !Term
}

data PathConstraint =
    Assume Term
  | XXX

data HeapState = HeapState {
  heapContext       :: !TermContext,
  -- XXX: valmap defs
  heapLocs          :: ![HeapLoc],
  heapConstraints   :: ![PathConstraint],
  heapNext          :: !Int
}

data Error = Error
type Solver = ()

newtype Heap a = Heap (Solver -> HeapState -> IO (Either Error a, HeapState))

instance Functor Heap where
  fmap = liftM
  {-# inline fmap #-}

instance Applicative Heap where
  pure a = Heap \_ s -> pure (Right a,s)
  (<*>) = ap
  {-# inline pure #-}
  {-# inline (<*>) #-}

instance Monad Heap where
  Heap m >>= k = Heap \r s ->
    do (mb,s1) <- m r s
       case mb of
         Left err -> pure (Left err, s1)
         Right a  ->
           let Heap m1 = k a
           in m1 r s1
  {-# inline (>>=) #-}

termB :: TermB () a -> Heap a
termB b = Heap \_ s ->
  let (res,newCtxt) =
        buildInContext () (heapContext s)
        do a <- b
           c <- getContext
           pure (a,c)
      !s1 = s { heapContext = newCtxt }
  in pure (Right res, s1)
{-# inline termB #-}



-- | ^ `(newLocs, missingPerms) <- remove locs (r,perms)`
-- After doing this we need to prove that `forall r. missingPerms == 0`.
remove ::
  [HeapLoc] {- ^ Relevant locations in the heap -} ->
  (# TVarName, Term #) {- ^ Function describing the permissions we need -} ->
  TermB s ([HeapLoc], Term)
  -- ^ Update heap locations, and any permissions we didn't get.
remove = go []
  where
  go done locs (# needVar, needed #) =
    case locs of
      [] -> pure (done, needed)
      loc : more ->
        do thisPerms <- renameVar (locName loc) needVar (locPerms loc)
           cond      <- term (TOp2 Lt thisPerms needed)
           smaller   <- term (TITE cond thisPerms needed)
           newNeeded <- term (TOp2 Sub needed smaller)
           smaller'  <- renameVar needVar (locName loc) smaller
           newPerms  <- term (TOp2 Sub thisPerms smaller')
           let newLoc = loc { locPerms = newPerms }
           go (newLoc : done) more (# needVar, newNeeded #)  