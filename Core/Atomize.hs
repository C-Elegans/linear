module Core.Atomize (atomize) where

import Core
import Core.Helper
import Weight
import Debug.Trace (trace)

{-
 - This module converts all expressions in case expressions and function applications
 - to atomic expressions (Variables)
 -}

atomize :: [Function] -> CompilerM [Function]
atomize = functionApply (descendM atomizeM)

atomizeM :: Expr Var -> CompilerM (Expr Var)
{-atomizeM e | trace (show e) False = undefined-}
atomizeM e@(App Var{} Var{}) = return e
{-
 - STG Case expressions can have an expression as their scrutinee,
 - and it is more efficient to do so because no thunk must be allocated.
 -}
{-atomizeM e@(Case (Var _) _ _ _) = return e-}
{-atomizeM (Case e b t a) = do-} 
    {-tv <- newTv e-}
    {-return (Let (NonRec tv e) (Case (Var tv) b t a))-}
atomizeM (App l e) = do
    tv <- newTv e
    f  <- newTv l
    return $! Let (NonRec tv e) $ Let (NonRec f l) (App (Var f) (Var tv))
atomizeM e@(Op b a1 a2)
    | isAtomic a1 && isAtomic a2 = return e
atomizeM e@(Op b a e2) 
    | isAtomic a = do
    tv <- newTv e2
    return $! Let (NonRec tv e2) $ Op b a (Var tv)
atomizeM e@(Op b e1 a) 
    | isAtomic a = do
    tv <- newTv e1
    return $! Let (NonRec tv e1) $ Op b (Var tv) a
atomizeM e@(Op b e1 e2) = do
    tv <- newTv e1
    tv2 <- newTv e2
    return $! Let (NonRec tv e1) $ Let (NonRec tv2 e2) $ Op b (Var tv) (Var tv2)
atomizeM x = return x

isAtomic :: Expr Var -> Bool
isAtomic Var{} = True
isAtomic Lit{} = True
isAtomic _     = False

newTv :: Expr Var -> CompilerM Var
newTv e = do
    fr <- fresh
    let name = "tmp_atom" ++ show fr
    return TyVar {varName=name, realUnique=fr,varType=exprType e, varWeight=Omega}
