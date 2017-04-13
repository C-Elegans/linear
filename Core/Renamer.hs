{-# Language TypeSynonymInstances, FlexibleInstances #-}
module Core.Renamer 
    where
import Core
import Pretty
import Debug.Trace (trace)
import Control.Monad.State
type RenameMonad = State Int

instance FreshMonad RenameMonad where
    fresh = do
        u <- get
        put $ u+1
        return u

rename :: Expr Var -> Expr Var
rename x = evalState (descendM renameM x) 0

renameM :: Expr Var -> RenameMonad (Expr Var)
renameM (Lam v e) = do
    fr <- fresh
    let v' = appendName v "_rn" fr 
    return $ Lam v' $ descend (replaceAllNames (varName v) (varName v') fr) e

renameM (Let (NonRec v b) e) = do
    fr <- fresh
    let v' = appendName v "_rn" fr
    return $ Let (NonRec v' b) $ descend (replaceAllNames (varName v) (varName v') fr) e
renameM (Let (Rec _) _) = return $ error "Recursive let not supported!"

renameM x = return x

replaceAllNames :: String -> String -> Int -> Expr Var -> Expr Var
replaceAllNames oldName newName i (Var v) 
    | varName v == oldName = Var $ v {varName = newName, realUnique = i}
replaceAllNames _ _ _ x =  x

    

appendName :: Var -> String -> Int -> Var
appendName v s i = v {varName = varName v ++ s ++ show i, realUnique = i}
