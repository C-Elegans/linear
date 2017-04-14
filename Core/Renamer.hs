{-# Language TypeSynonymInstances, FlexibleInstances #-}
module Core.Renamer 
    where
import Core
import Core.Helper
import Pretty
import Debug.Trace (trace)
import Control.Monad.State
{-
 - The renamer module resolves any name shadowing in its inputs by renaming all 
 - variables with unique names
-}


rename :: [Function] -> CompilerM [Function]
rename = mapM rn 
    where
    rn :: Function -> CompilerM Function
    rn f = do 
        let b = fBody f
        b' <- descendM renameM b 
        return $ f {fBody = b'}

renameM :: Expr Var -> CompilerM (Expr Var)
renameM (Lam v@TyVar{} e) = do
    fr <- fresh
    let v' = appendName v "_rn" fr 
    return $ Lam v' $ descend (replaceAllNames (varName v) (varName v') fr) e

renameM (Let (NonRec v@TyVar{} b) e) = do
    fr <- fresh
    let v' = appendName v "_rn" fr
    return $ Let (NonRec v' b) $ descend (replaceAllNames (varName v) (varName v') fr) e
renameM (Let (Rec _) _) = return $ error "Recursive let not supported!"

renameM x = return x

replaceAllNames :: String -> String -> Int -> Expr Var -> Expr Var
replaceAllNames oldName newName i (Var v@TyVar{}) 
    | varName v == oldName = Var $ v {varName = newName, realUnique = i}
replaceAllNames _ _ _ x =  x

    

appendName :: Var -> String -> Int -> Var
appendName v s i = v {varName = varName v ++ s ++ show i, realUnique = i}
