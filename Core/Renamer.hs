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

rename :: [Function] -> [Function]
rename = map rn 
    where
    rn :: Function -> Function
    rn f = 
        let b = fBody f
            b' = evalState (descendM renameM b) 0
        in f {fBody = b'}

renameM :: Expr Var -> RenameMonad (Expr Var)
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
