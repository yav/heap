module BuildTerm
  ( -- * Building terms
    term, build, TermB
   
  -- * Context
  , buildInContext
  , getContext
  , TermContext 

  -- * User state
  , getUser, setUser, updUser
  , withUser, liftUser
  )
  where

import Data.Map.Strict(Map)
import Data.Map.Strict qualified as Map
import Control.Monad(liftM,ap)

import PrimTerm
import EvalTerm

type KnownTerms = Map (TermF Term) Term

data TermContext = TermContext
  { ctxtKnown :: KnownTerms
  , ctxtNext  :: Int
  }

buildInContext :: s -> TermContext -> TermB s a -> a
buildInContext s TermContext { ctxtNext, ctxtKnown } (TermB m) =
  case m ctxtNext ctxtKnown s of
    (# a, _, _, _ #) -> a
{-# INLINE buildInContext #-}

build :: s -> TermB s a -> a
build s = buildInContext s TermContext { ctxtKnown = mempty, ctxtNext = 0 }
{-# INLINE build #-}


newtype TermB s a = TermB (Int -> KnownTerms -> s -> (# a, Int, KnownTerms, s #))

instance Functor (TermB s) where
  fmap = liftM
  {-# INLINE fmap #-}

instance Applicative (TermB s) where
  pure a = TermB \i k s -> (# a, i, k, s #)
  {-# inline pure #-}
  (<*>) = ap
  {-# inline (<*>) #-}

instance Monad (TermB s) where
  TermB m >>= f = TermB \i k s ->
    case m i k s of
       (# a, i1, k1, s1 #) ->
          let TermB m2 = f a
          in m2 i1 k1 s1
  {-# INLINE (>>=) #-}

term :: TermF Term -> TermB s Term
term tf = TermB \i known s ->
  case Map.lookup tf known of
    Just t -> (# t, i, known, s #)
    Nothing ->
      let TermB m = evalBuild doBuild tf
      in m i known s

doBuild :: TermF Term -> TermB s Term
doBuild tf = TermB \i known s ->
  let t       = Term { termId = i, termF = tf }
      !i1     = i + 1
      !known1 = Map.insert tf t known
  in (# t, i1, known1, s #)

getContext :: TermB s TermContext
getContext = TermB \i known s ->
  (# TermContext { ctxtKnown = known, ctxtNext = i }, i, known, s #)
{-# INLINE getContext #-}

getUser :: TermB s s
getUser = TermB \i k s -> (# s, i, k, s #)
{-# INLINE getUser #-}

setUser :: s -> TermB s ()
setUser s = TermB \i k _ -> (# (), i, k, s #)
{-# INLINE setUser #-}

updUser :: (s -> s) -> TermB s ()
updUser f = TermB \i k s -> 
 let !s1 = f s
 in (# (), i, k, s1 #)
{-# INLINE updUser #-}

withUser :: s1 -> TermB (s1,s) a -> TermB s a
withUser s1 (TermB m) = TermB \i k s ->
  case m i k (s1,s) of
    (# a, i1, k1, (_,s') #) -> (# a, i1, k1, s' #)
{-# INLINE withUser #-}

liftUser :: TermB s a -> TermB (s1,s) a
liftUser (TermB m) = TermB \i k (s1,s) ->
  case m i k s of
    (# a, i', k', s' #) -> (# a, i', k', (s1,s') #)
{-# INLINE liftUser #-}