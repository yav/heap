
module Term
  ( T.Type(..)
  , T.TermF(..)
  , T.TVarName(..), T.TFunName(..)
  , T.Op1(..), T.Op2(..)
  , T.Term, termId, termF
  , T.TypeOf(..)
  ) where

import PrimTerm qualified as T


termId :: T.Term -> Int
termId = T.termId

termF :: T.Term -> T.TermF T.Term
termF = T.termF