module Core.Atomize (atomize) where

import Core
import Core.Helper
import Weight
import Debug.Trace (trace)
import Type

{-
 - This module converts all expressions in case expressions and function applications
 - to atomic expressions (Variables)
 -}

atomize :: [Function] -> CompilerM [Function]
atomize = functionApply (descendM atomizeM)

atomizeM :: Expr Var -> CompilerM (Expr Var)
{-atomizeM e | trace (show e) False = undefined-}
atomizeM e@(App v1 v2)
    | isAtomic v1 && isAtomic v2 = return e
{-
 - STG Case expressions can have an expression as their scrutinee,
 - and it is more efficient to do so because no thunk must be allocated.
 -}
{-atomizeM (Case e b t a) = do-} 
    {-tv <- newTv e-}
    {-return (Let (NonRec tv e) (Case (Var tv) b t a))-}
atomizeM (App (Var v) e) = 
    case varType v of
    TyConApp _ _ ->
        return $! (App (Var v) e) 
    _ -> do
        tv <- newTv e
        return $! Let (NonRec tv e) (App (Var v) (Var tv))
atomizeM (App l v) 
    | isAtomic v = do
    f <- newTv l
    return $! Let (NonRec f l) (App (Var f) v)
atomizeM (App l e) = do
    tv <- newTv e
    f  <- newTv l
    return $! Let (NonRec tv e) $ Let (NonRec f l) (App (Var f) (Var tv))
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
