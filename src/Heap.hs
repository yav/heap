module Heap where

import Control.Monad(liftM,ap)
import Data.IntMap(IntMap)
import Data.IntMap qualified as IntMap

import Term
import BuildTerm
import Subst


data PathConstraint =
    Assume Term
  | XXX

-- | Facts about a particular allocation.
data Allocation = Allocation {
  locVal        :: !Term,       -- ^ Value of element
  locAccessible :: !Term,       -- ^ Can we access location
  locWrite      :: !Term,       -- ^ If the location was accessible, could we write to it. 
  locRead       :: !Term        -- ^ If the location was accessible, could we read from it.
}

-- | What kind of allocation we have.
data AllocArray = AllocMany {
  locIndex  :: !TVarName,
  locAllocs :: [Allocation]
}

-- | The state of the heap.
data HeapState = HeapState {
  heapContext       :: !TermContext,
  -- XXX: valmap defs
  heapAllocs        :: !(IntMap AllocArray),
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

getState :: Heap HeapState
getState = Heap \_ s -> pure (Right s, s)
{-# inline getState #-}

setState :: HeapState -> Heap ()
setState s = Heap \_ _ -> pure (Right (), s)
{-# inline setState #-}

termB :: TermB () a -> Heap a
termB b = 
  do s <- getState
     let (res,newCtxt) =
           buildInContext () (heapContext s)
           do a <- b
              c <- getContext
              pure (a,c)
         !s1 = s { heapContext = newCtxt }
     setState s1
     pure res
{-# inline termB #-}



{-
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




removeAccessAlloc ::
  Allocation {- ^ Facts about an allocation -} -> 
  Term       {- ^ Identifies what to remove access to -} ->
  TermB s (Allocation, Term)
  -- ^ Updated allocation, and additional access we need.
removeAccessAlloc loc needed =
  do let have = locAccessible loc
     notNeeded <- term (TOp1 Not needed)
     newHave   <- term (TOp2 And have notNeeded)
     notHave   <- term (TOp1 Not have)
     newNeeded <- term (TOp2 And needed notHave)
     let !newLoc = loc { locAccessible = newHave }
     pure (newLoc, newNeeded)

removeAccessAllocs :: [Allocation] -> [Allocation] -> Term -> TermB s ([Allocation], Term)
removeAccessAllocs done todo needed =
  case todo of
     [] -> pure (done, needed)
     a : more ->
      do (a', newNeeded) <- removeAccessAlloc a needed
         removeAccessAllocs (a' : done) more newNeeded


removeArrayAccess :: Int -> (TVarName -> TermB () Term) -> Heap ()
removeArrayAccess n perms =
  do s <- getState
     case IntMap.lookup n (heapAllocs s) of
       Just as ->
         do need <- termB (perms (locIndex as))
            (as', missing) <- termB (removeAccessAllocs [] (locAllocs as) need)
            -- XXX: check forall (locIndex as). !missing
            let !newAll = as { locAllocs = as' } 
                !newHeap = s { heapAllocs = IntMap.insert n newAll (heapAllocs s) }
            setState newHeap
              
       Nothing -> error "removeAccess: invalid allocation"
