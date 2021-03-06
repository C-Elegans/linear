module Stg.CoreToStg where
import Core
import Core.Helper
import DataCon
import Type
import Stg
import Data.List
import Debug.Trace (trace)
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
coreExprToStg (App (Var v) a) = 
    case isConstr v of
        True -> 
            case getDataCon v of
                Just con -> return $! GenStgConApp con [toArg a] (dcOrigArgTys con) 
                Nothing -> fail $ "No datacon found for " ++ show v 
        _    -> return $! GenStgApp v [toArg a]
coreExprToStg (Lam v1 v2) = GenStgLam [v1] <$> coreExprToStg v2
coreExprToStg (Let (NonRec v e) i) = do
    e' <- coreExprToStg e
    i' <- coreExprToStg i
    let frees = freeVars e'
    let rhs = stgSimp $ StgRhsClosure NoStgBinderInfo frees [] e'
    return $! GenStgLet (StgNonRec v rhs) i'
coreExprToStg (Let (Rec v1) i) = do
    rhs <- mapM (\ (bnd,e) -> do
        e' <- coreExprToStg e
        return (bnd, StgRhsClosure NoStgBinderInfo [] [] e')) v1
    i' <- coreExprToStg i 
    return $! GenStgLet (StgRec rhs) i'
coreExprToStg (Op op e1 e2) = return $! GenStgOpApp op (map toArg [e1,e2]) (exprType e1)
coreExprToStg (Case v1 bndr ty alts) = do
    scrut <- coreExprToStg v1
    let altType = PrimAlt
    alts <- mapM coreAltToStg alts
    return $! GenStgCase scrut bndr altType alts 
    

{-coreExprToStg (Type v) = undefined-}
coreExprToStg x = return $ error $ "No coreExprToStg defined for " ++ show x

toArg :: Expr Var -> StgArg
toArg (Var v) = StgVarArg v
toArg (Lit l) = StgLitArg l
toArg x = error $ "Cannot convert expr: " ++ show x ++ "to StgArg (CoreToStg.hs:50)" 

stgSimp :: StgRhs -> StgRhs
stgSimp r@(StgRhsClosure info occ bndr (GenStgLam vs e)) = 
    stgSimp $! StgRhsClosure info occ (bndr ++ vs) e
stgSimp x = x

freeVars :: StgExpr -> [Var]
freeVars (GenStgApp fr e) = fr:concatMap freeVarsInArg e
freeVars (GenStgLit e) = []
freeVars (GenStgConApp e1 e2 e3) = undefined
freeVars (GenStgOpApp e1 e2 e3) = concatMap freeVarsInArg e2
freeVars (GenStgLam pars e) = freeVars e \\ pars
freeVars (GenStgCase e1 e2 e3 e4) = undefined
freeVars (GenStgLet (StgNonRec bnd (StgRhsClosure info free bnds ex)) i) = 
    free ++ freeVars i

freeVarsInArg :: StgArg -> [Var]
freeVarsInArg (StgVarArg a) = [a]
freeVarsInArg (StgLitArg a) = []

coreAltToStg :: Alt Var -> CompilerM StgAlt
coreAltToStg (con,vars,e) = (,,) <$> pure con <*> pure vars <*> coreExprToStg e

