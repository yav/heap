module Heap where

import Term

data HeapLoc = HeapEntry
  { locName   :: !TVarName
  , locVal    :: !Term
  , locPerms  :: !Term
  }