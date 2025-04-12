module PPTerm (
  PP(..),
  PPConfig,
  ppDefault,
  ppWithTypes
) where

import Prelude hiding ((<>))
import Data.Ratio(numerator,denominator)
import Text.PrettyPrint
import Term
import FoldTerm

data PPConfig = PPConfig {
  withTypes :: Bool
}

ppDefault :: PPConfig
ppDefault = PPConfig {
  withTypes = False
}

ppWithTypes :: Bool -> PPConfig -> PPConfig
ppWithTypes y c = c { withTypes = y }

class PP t where
  pp :: (?ppConfig :: PPConfig) => t -> Doc

instance PP TVarName where
  pp x = withTypeMaybe ("x" <> int (tvarId x)) (tvarType x)

instance PP TFunName where
  pp f = withTypeMaybe ("F" <> int (tfunId f)) (tfunResType f)

withTypeMaybe :: (?ppConfig :: PPConfig) => Doc -> Type -> Doc
withTypeMaybe d t
  | withTypes ?ppConfig = d <> ":" <+> pp t
  | otherwise = d

-- lower number is higher precedence
opPrec :: (?ppConfig :: PPConfig) => TermF a -> (Doc, Int)
opPrec term
  | withTypes ?ppConfig = (doc, 14)
  | otherwise = (doc,prec)
  where
  (doc,prec) =
    case term of
      TVar {} -> ("TVar",-1)
      TBool {} -> ("TBool",-1)
      TInt {} -> ("TInt",-1)
      TRat {} -> ("TRat",-1)
      TOp1 op _ ->
        case op of
          Not    -> ("!",2)
          Neg    -> ("-",2)
          App1 f -> (pp f,2)
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
          ArrayGet -> ("[]",1)
      TOp3 op _ _ _ ->
        case op of
          ITE -> ("?",13)
          ArraySet -> ("[=]",1)

instance PP Type where
  pp t =
    case t of
      Integer  -> "int"
      Boolean  -> "bool"
      Rational -> "ratio"
      Array el -> pp el <> "[]"
      Location -> "pointer"

instance PP Term where
  pp = foldTerm \self selfF ->
    case selfF of
      TVar x     -> pp x
      TBool x    -> if x then "true" else "false"
      TInt n     -> integer n
      TRat r     -> braces (integer (numerator r) <> comma <+> integer (denominator r))
      TOp1 (App1 f) d -> withTypeMaybe (fu <> parens d) (typeOf f)
        where fu = let ?ppConfig = ppWithTypes False ?ppConfig
                   in pp f 
      TOp1 _ d ->
        let (opD,mine) = opPrec selfF
            t          = case termF self of
                           TOp1 _ a -> a
                           _        -> error "ppTerm: self mismatch TOp1"
            (_,sub)    = opPrec (termF t)
        in opD <+> if sub > mine then parens d else d
  
      TOp2 op d1 d2 ->
        case op of
          ArrayGet -> d1 <> brackets d2
          _ ->
            let (opD, mine) = opPrec selfF
                (t1,t2)     = case termF self of
                                TOp2 _ a b -> (a,b)
                                _          -> error "ppTerm: self mismatch TOp2"
                (_, subL)   = opPrec (termF t1)
                (_, subR)   = opPrec (termF t2)
                d1'         = if subL > mine  then parens d1 else d1
                d2'         = if subR >= mine then parens d2 else d2
            in hang d1' 2 (opD <+> d2')
  
      TOp3 op d1 d2 d3 ->
        case op of
          ArraySet -> d1 <> brackets (d2 <+> "=" <+> d3)
          ITE ->
            let (_, mine) = opPrec selfF
                (t1,t3)   = case termF self of
                              TOp3 _ a _ b -> (a,b)
                              _            -> error "ppTerm: self mismatch TITE"
                (_, p1)   = opPrec (termF t1)
                (_, p3)   = opPrec (termF t3)
                d1'       = if p1 >= mine then parens d1 else d1
                d3'       = if p3 >  mine then parens d3 else d3
            in hang d1' 2 (hang ("?" <+> d2) 2 (":" <+> d3'))
  

