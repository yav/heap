{-# Language OverloadedStrings, BlockArguments, LambdaCase #-}
module PPTerm
  ( ppTVarName
  , ppTerm
  ) where

import Prelude hiding ((<>))
import Text.PrettyPrint
import SimpleTerm
import FoldTerm

ppTVarName :: TVarName -> Doc
ppTVarName (TVarName x) = "x" <> int x

-- lower number is higher precedence
opPrec :: TermF a -> (Doc, Int)
opPrec term =
  case term of
    TVar {} -> ("TVar",-1)
    TBool {} -> ("TBool",-1)
    TInt {} -> ("TInt",-1)
    TOp1 op _ ->
      case op of
        Not -> ("!",2)
        Neg -> ("-",2)
    TOp2 op _ _ ->
      case op of
        Add -> ("+",4)
        Sub -> ("-",4)
        Mul -> ("*",3)
        Div -> ("/",3)
        Mod -> ("%",3)
        Eq  -> ("==",7)
        Leq -> ("<=",6)
        Lt  -> ("<",6)
        And -> ("&&",11)
        Or  -> ("||",10)
    TITE {} -> ("?",13)

ppTerm :: Term -> Doc
ppTerm = fold' \case
    TVar x     -> ppTVarName x
    TBool x    -> if x then "true" else "false"
    TInt n     -> integer n
    self@(TOp1 _ (Term t,d)) ->
      let (opD,mine) = opPrec self
          (_,sub)    = opPrec t
      in opD <+> if sub > mine then parens d else d

    self@(TOp2 _ (Term t1,d1) (Term t2,d2)) ->
        let (opD, mine) = opPrec self
            (_, subL)   = opPrec t1
            (_, subR)   = opPrec t2
            d1'         = if subL > mine  then parens d1 else d1
            d2'         = if subR >= mine then parens d2 else d2
        in hang d1' 2 (opD <+> d2')

    self@(TITE (Term t1, d1) (Term _, d2) (Term t3, d3)) ->
      let (_, mine) = opPrec self
          (_, p1)   = opPrec t1
          (_, p3)   = opPrec t3
          d1'       = if p1 >= mine then parens d1 else d1
          d3'       = if p3 >  mine then parens d3 else d3
      in hang d1' 2 (hang ("?" <+> d2) 2 (":" <+> d3'))


