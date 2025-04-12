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

{-
Heap
====

An allocation for an array of `a`s (1 is a special case).
XXX: instead of using implicit indexing and index-element isomorphism as in paper
we can just user arrays.

{ base:       Address     -- the base of the object (i.e., its identity)
, index:      TermVar     -- Binder.  In scope in the following 3 fields/
, value:      Term (a)    -- keeps track of the location stored here
, accessible: Term (bool) -- what parts are known to exist (i.e, can be written)
, readable:   Term (bool) -- what parts are known to be readable (e.g., are initialized)
}
Notes:
  * to read something we need *both* accessible and readable.
  * to work with slices (i.e., a sub array), we have to adjust the indexed as needed.

Example (ignore `readbale` for simplcity, and index is always `i`)

Source:
  f(p: pointer);
    require: a <- array(p,5)
    ensure: b <- array(p,5); assert(a == b)
  
  g(q: pointer) -> int;
    require: c <- array(q,10)
    ensure: d <- array(q,10); assetr (c == d); assert (return == c[0] + c[6])
  { f(q);
    q[0] + q[6]
  }

Reasoning:

require a <- array(p,10)
  X: { base: q, value: c, accessible: i < 10 }

call f
  exhale(a <- array(q,5))
    { let p = q }
    check: forall i. (q == p && i < 5) => X.accessible
     i.e.: forall i. (i < 5) => (i < 10)
    update: X: { base: q, value c, accessible: i < 10 && not (i < 5)
    { let a = c } 
  inhale(b <- array(p,5); assert (a == b))
    Y: { base: p, value: b, accsible: i < 5 }
    assert (b == c)

new state:
  heap:
  X: { base: q, value: c, accessible: i < 10 && not (i < 5) }
  Y: { base: q, value: b, accessible: i < 5 }
  path:
  b == c

read q[0]
  v = summarize(q[0])
  v = if (q == q && 0 < 10 && not (0 < 5)) c[0] else
      if (q == q && 0 < 5) b[0]
  i.e. v = b[0]

Pointers
========

A pointer is a pair of an allocation identifier and a "path" in it.

If we use a structured memory model paths would be something like this:

data Path =
    Here
  | FieldOf Path Label
  | IndexOf Path Term

For a flat "byte oriented" models, paths would be just an index (i.e., a Term)
-}



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
           smaller   <- term (TOp3 ITE cond thisPerms needed)
           newNeeded <- term (TOp2 Sub needed smaller)
           smaller'  <- renameVar needVar (locName loc) smaller
           newPerms  <- term (TOp2 Sub thisPerms smaller')
           let newLoc = loc { locPerms = newPerms }
           go (newLoc : done) more (# needVar, newNeeded #)  