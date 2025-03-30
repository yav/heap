module Solver
  ( Solver,
    newSolver,
    push,
    pop,
    assume,
    isValid
  ) where

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Foldable(traverse_)
import Data.IORef
import SimpleSMT qualified as SMT
import Term
import FoldTerm(foldTerm)

newSolver :: SMT.Config -> IO Solver
newSolver cfg =
  do s <- SMT.newSolverWithConfig cfg
     c <- newIORef (mempty,[])
     pure Solver { solver = s, context = c }

push :: Solver -> IO ()
push s =
  do SMT.push (solver s)
     modifyIORef (context s) (\(x,xs) -> (x,x:xs))

pop :: Solver -> IO ()
pop s =
  do SMT.pop (solver s)
     modifyIORef (context s) \(_,xs) ->
       case xs of
         a : as -> (a,as)
         _      -> error "pop: []"

addTerm :: Solver -> Term -> IO SMT.SExpr
addTerm s t =
  do let SMT { smt, decls } = smtTerm t
     new <- atomicModifyIORef' (context s) \(c,cs) ->
       let new = diff decls c
       in ((new <> c, cs), new)
     let so = solver s
     declare so new
     pure smt

assume :: Solver -> Term -> IO ()
assume s t =
  do sm <- addTerm s t
     SMT.assert (solver s) sm

isValid :: Solver -> Term -> IO (Maybe Bool)
isValid s t =
  do sm <- addTerm s t
     let so = solver s
     SMT.inNewScope so
       do SMT.assert so (SMT.not sm)
          res <- SMT.check so
          pure
            case res of
              SMT.Unsat   -> Just True
              SMT.Sat     -> Just False
              SMT.Unknown -> Nothing 

data Solver = Solver {
  solver   :: SMT.Solver,
  context  :: IORef (Declared, [Declared])
}

data Declared = Declared {
  dVars  :: IntMap (String,SMT.SExpr),           -- ^ (var,ty)
  dFuns  :: IntMap (String,SMT.SExpr,SMT.SExpr), -- ^ (fun,argT,resT)
  dTerms :: IntMap (String,SMT.SExpr,SMT.SExpr)  -- ^ (term,def,ty)
}

instance Semigroup Declared where
  xs <> ys = Declared {
    dVars = dVars xs <> dVars ys,
    dFuns = dFuns xs <> dFuns ys,
    dTerms = dTerms xs <> dTerms ys
  }

instance Monoid Declared where
  mempty = Declared {
    dVars = mempty,
    dFuns = mempty,
    dTerms = mempty
  }

diff :: Declared -> Declared -> Declared
diff xs ys = Declared {
  dVars = IntMap.difference (dVars xs) (dVars ys),
  dFuns = IntMap.difference (dFuns xs) (dFuns ys),
  dTerms = IntMap.difference (dTerms xs) (dTerms ys)
}

declare :: SMT.Solver -> Declared -> IO ()
declare s ds =
  do doMap doVar dVars
     doMap doFun dFuns
     doMap doTerm dTerms
  where
  doMap fu fi     = traverse_ fu (fi ds) 
  doVar (x,t)     = SMT.declare s x t
  doFun (f,tA,tR) = SMT.declareFun s f [tA] tR
  doTerm (t,d,ty) = SMT.define s t ty d 


data SMT = SMT {
  smt       :: !SMT.SExpr,
  ty        :: !Type,
  decls     :: !Declared
}

mkLit :: Term -> SMT.SExpr -> SMT
mkLit self d = SMT {
  smt = d,
  ty = typeOf self,
  decls = mempty
}

mkDef :: Term -> SMT.SExpr -> Declared -> SMT
mkDef self d ds =
  let i = termId self
      t = typeOf self
      !nm = smtTermName i
      !ts = smtType t
  in SMT {
    smt  = SMT.const nm,
    ty   = t,
    decls = ds { dTerms = IntMap.insert i (nm,d,ts) (dTerms ds) }
}

smtTerm :: Term -> SMT
smtTerm = foldTerm \self tf ->
  case tf of
      
      TVar x -> SMT {
        smt   = SMT.const nm,
        ty    = t,
        decls = mempty { dVars = IntMap.singleton (tvarId x) (nm,smtType t) }
      } where
        nm = smtVarName x
        t = typeOf x

      TBool b -> mkLit self (SMT.bool b)
      TInt n  -> mkLit self (SMT.int n)
      TRat r  -> mkLit self (SMT.real r)

      TOp1 op t     -> op1 self op t
      TOp2 op t1 t2 -> op2 self op t1 t2 
      TITE c t1 t2 ->
        mkDef self (SMT.ite (smt c) (smt t1) (smt t2))
                   (decls c <> decls t1 <> decls t2)


smtType :: Type -> SMT.SExpr
smtType ty =
  case ty of
    Boolean  -> SMT.tBool
    Integer  -> SMT.tInt
    Rational -> SMT.tReal
    Location -> error "Location: XXX" 
 
smtVarName :: TVarName -> String
smtVarName x = "x" ++ show (tvarId x)

smtFunName :: TFunName -> String
smtFunName f = "f" ++ show (tfunId f)

smtTermName :: Int -> String
smtTermName n = "t" ++ show n

op1 :: Term -> Op1 -> SMT -> SMT
op1 self op arg =
  case op of
    Neg    -> mkDef self (SMT.neg (smt arg)) (decls arg) 
    Not    -> mkDef self (SMT.not (smt arg)) (decls arg)
    App1 f -> mkDef self (SMT.app (SMT.const nm) [smt arg])
                  ds { dFuns = IntMap.insert (tfunId f) (nm,argT,argR) (dFuns ds) }
      where !nm   = smtFunName f
            ds    = decls arg
            !argT = smtType (ty arg)
            !argR = smtType (typeOf f) 

op2 :: Term -> Op2 -> SMT -> SMT -> SMT
op2 self op arg1 arg2 =
  mkDef self (fu (smt arg1) (smt arg2)) (decls arg1 <> decls arg2)
  where
  fu =
    case op of
      Add -> SMT.add
      Sub -> SMT.sub
      Mul -> SMT.mul
      Div -> SMT.div
      Mod -> SMT.mod
      Eq  -> SMT.eq
      Leq -> SMT.leq
      Lt  -> SMT.lt
      And -> SMT.and
      Or  -> SMT.or
  