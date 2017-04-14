{-# Language TypeSynonymInstances, FlexibleInstances #-}
module Core.Simplify(
    runSimplify
    ) where
import Core
import Core.Helper
import Pretty (pp)
import Debug.Trace (trace)
import Control.Monad.State

runSimplify :: [Function] -> CompilerM [Function]
runSimplify = mapM rs
    where 
    rs f = do 
        body <- descendM simplifyPasses (fBody f)
        return $ f {fBody = body}

simplifyPasses :: Expr Var -> CompilerM (Expr Var)
simplifyPasses e = 
    constProp e >>=
    removeUnused
    
-- Replaces constant variables in let bindings with the constant
constProp :: Expr Var -> CompilerM (Expr Var)
{-constProp e@(Let _ _) | trace (show e) False = undefined-}
constProp (Let (NonRec v l@(Lit _)) e) = do
    let e' = descend (replaceAllVars v l) e
    return e'
constProp x = exprSimplify x

-- Simplifies constant expressions
exprSimplify :: Expr Var -> CompilerM (Expr Var)
{-exprSimplify e | trace (pp False e) False = undefined-}
exprSimplify (Op op e1 e2) = do
    e1' <- exprSimplify e1
    e2' <- exprSimplify e2
    if bothInt e1' e2' then
        return $ eval op e1' e2'
    else
        return (Op op e1' e2')
    where
        bothInt :: Expr Var -> Expr Var -> Bool
        bothInt (Lit (Int _)) (Lit (Int _)) = True
        bothInt _ _ = False
        eval :: Binop -> Expr Var -> Expr Var -> Expr Var
        eval op (Lit (Int i1)) (Lit (Int i2)) = Lit $ Int $ opFromOp op i1 i2
        eval op _ _ = error $ "invalid operation " ++ show op
        opFromOp Add = (+)
        opFromOp Sub = (-)
        opFromOp Mul = (*)
exprSimplify x = return x

--removes unused variables 
removeUnused :: Expr Var -> CompilerM (Expr Var)
removeUnused (Lam v@TyVar{} e) 
    | varUnused v e = return (Lam Hole e)
removeUnused (Let (NonRec v@TyVar{} b) e)
    | varUnused v e = return e
removeUnused l = return l

varUnused v (Var v2) = v/=v2
varUnused v (Lit _) = True
varUnused v (App e b) = varUnused v e && varUnused v b
varUnused v (Lam _ e) = varUnused v e
varUnused v (Let (NonRec _ b) e) = varUnused v b && varUnused v e
varUnused v (Op _ e1 e2) = varUnused v e1 && varUnused v e2
varUnused v _ = False

