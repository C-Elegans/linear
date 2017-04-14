module Core.ReduceLinear where
import Core
import Core.Helper
import Weight
import Debug.Trace (trace)

reduceLinear :: [Function] -> CompilerM [Function]
reduceLinear = mapM rl
    where
    rl :: Function -> CompilerM Function
    rl f = do
        b <- descendM strengthreduce (fBody f)
        return $ f {fBody=b}
strengthreduce :: Expr Var -> CompilerM (Expr Var)
strengthreduce (Lam v@TyVar{} e) 
    | countOccurences v e == 1 = do
        let newVar = v {varWeight = One}
        return $ Lam newVar (descend (replaceAllVars v (Var newVar)) e)
strengthreduce (Let (NonRec v@TyVar{} b) e) 
    | countOccurences v e == 1 = do
        let newVar = v {varWeight = One}
        return $ Let (NonRec newVar b) (descend (replaceAllVars v (Var newVar)) e)
strengthreduce e = return e

countOccurences :: Var -> Expr Var -> Int
countOccurences v (Var v2) | v == v2 = 1
countOccurences v (App e a) = countOccurences v e + countOccurences v a
countOccurences v (Lam v2 e) = if v == v2 then 
    1 + countOccurences v e else countOccurences v e
countOccurences v (Let (NonRec _ b) e) = countOccurences v b + countOccurences v e
countOccurences v (Op _ e1 e2) = countOccurences v e1 + countOccurences v e2
countOccurences _ (Lit _) = 0
countOccurences _ _ = 0
