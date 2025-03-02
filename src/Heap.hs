module Heap where

import Control.Monad(liftM,ap)

import Term
import BuildTerm

data HeapLoc = HeapEntry {
  locVal    :: !TVarName,
  locPerms  :: !(Term -> TermB () Term)
}

data PathConstraint =
    Assume Term
  | XXX

data HeapState = HeapState {
  heapContext       :: !TermContext,
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
  

-- The complexity of this looks pretty bad
remove :: [HeapLoc] -> (Term -> TermB () Term) -> ([HeapLoc], Term -> TermB () Term)
remove = go []
  where
  go done locs needed =
    case locs of
      [] -> (done, needed)
      loc : more -> go (newLoc : done) more newNeeded  
        where
        cur r =
          do permT <- locPerms loc r
             needT <- needed r
             cond  <- term (TOp2 Lt permT needT)
             smaller <- term (TITE cond permT needT)
             pure (smaller, permT, needT)
        newNeeded r =
          do (curT, _, needT) <- cur r
             term (TOp2 Sub needT curT)
        newPerms r =
          do (curT, permT, _)  <- cur r
             term (TOp2 Sub permT curT)
        newLoc = loc { locPerms = newPerms }