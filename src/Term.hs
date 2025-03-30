
module Term
  ( T.Type(..)
  , T.TermF(..)
  , T.TVarName, tvarId, tvarType
  , T.TFunName, tfunId, tfunResType
  , T.Op1(..), T.Op2(..)
  , T.Term, termId, termF
  , T.TypeOf(..)
  ) where

import PrimTerm qualified as T

termId :: T.Term -> Int
termId = T.termId

termF :: T.Term -> T.TermF T.Term
termF = T.termF

tvarId :: T.TVarName -> Int
tvarId (T.TVarName x _) = x

tvarType :: T.TVarName -> T.Type
tvarType (T.TVarName _ t) = t

tfunId :: T.TFunName -> Int
tfunId (T.TFunName x _) = x

tfunResType :: T.TFunName -> T.Type
tfunResType (T.TFunName _ t) = t