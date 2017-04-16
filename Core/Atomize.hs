module Core.Atomize (atomize) where

import Core
import Core.Helper
import Weight

{-
 - This module converts all expressions in case expressions and function applications
 - to atomic expressions (Variables)
 -}

atomize :: [Function] -> CompilerM [Function]
atomize = functionApply (descendM atomizeM)

atomizeM :: Expr Var -> CompilerM (Expr Var)
atomizeM e@(Case (Var _) _ _ _) = return e
atomizeM e@(App _ Var{}) = return e
atomizeM (Case e b t a) = do
    tv <- newTv e
    return (Let (NonRec tv e) (Case (Var tv) b t a))
atomizeM (App l e) = do
    tv <- newTv e
    f  <- newTv l
    return $! Let (NonRec tv e) $ Let (NonRec f l) (App l (Var tv))
atomizeM x = return x

newTv :: Expr Var -> CompilerM Var
newTv e = do
    fr <- fresh
    let name = "tmp_atom" ++ show fr
    return TyVar {varName=name, realUnique=fr,varType=exprType e, varWeight=Omega}
