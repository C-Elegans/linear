module Stg.CoreToStg where
import Core
import Core.Helper
import Stg
coreToStg :: [Function] -> CompilerM [StgTopBinding]
coreToStg = mapM funcToStg 
    where
    funcToStg :: Function -> CompilerM StgTopBinding
    funcToStg f = do
        let body = fBody f
        newExpr <- coreExprToStg body 
        let v       = mkVar (fType f) (fName f)
        let rhs     = stgSimp $ StgRhsClosure NoStgBinderInfo [] [] newExpr
        let binding = StgNonRec v rhs
        return $! StgTopLifted binding

coreExprToStg :: Expr Var -> CompilerM StgExpr
coreExprToStg (Var v) = return $! GenStgApp v []
coreExprToStg (Lit l) = return $! GenStgLit l 
coreExprToStg (App (Var v) (Var a)) = return $! GenStgApp v [StgVarArg a]
coreExprToStg (Lam v1 v2) = GenStgLam [v1] <$> coreExprToStg v2
coreExprToStg (Let (NonRec v e) i) = do
    e' <- coreExprToStg e
    i' <- coreExprToStg i
    return $! GenStgLet (StgNonRec v (StgRhsClosure NoStgBinderInfo [] [] e')) i'
coreExprToStg (Let (Rec v1) i) = do
    rhs <- mapM (\ (bnd,e) -> do
        e' <- coreExprToStg e
        return (bnd, StgRhsClosure NoStgBinderInfo [] [] e') v1
    i' <- coreExprToStg i 
    return $! GenStgLet rhs i'
coreExprToStg (Op op e1 e2) = return $! GenStgOpApp op (map toArg [e1,e2]) (exprType e1)
{-coreExprToStg (Case v1 v2 v3 v4) = undefined-}
{-coreExprToStg (Type v) = undefined-}
coreExprToStg x = return $ error $ "No coreExprToStg defined for " ++ show x

toArg :: Expr Var -> StgArg
toArg (Var v) = StgVarArg v
toArg (Lit l) = StgLitArg l

stgSimp :: StgRhs -> StgRhs
stgSimp r@(StgRhsClosure info occ bndr (GenStgLam vs e)) = 
    stgSimp $! StgRhsClosure info occ (bndr ++ vs) e
stgSimp x = x
