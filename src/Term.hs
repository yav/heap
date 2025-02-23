
module Term
  ( T.TermF(..)
  , T.TVarName(..), T.Op1(..), T.Op2(..)
  , T.Term, termId, termF
  ) where

import PrimTerm qualified as T


termId :: T.Term -> Int
termId = T.termId

termF :: T.Term -> T.TermF T.Term
termF = T.termF