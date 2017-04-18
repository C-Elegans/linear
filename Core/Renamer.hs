{-# Language TypeSynonymInstances, FlexibleInstances #-}
module Core.Renamer 
    where
import Core
import Core.Helper
import DefaultTypes
import Pretty
import Data.List (foldl')
import Debug.Trace (trace)
import Control.Monad.State
import qualified Data.Text as T
{-
 - The renamer module resolves any name shadowing in its inputs by renaming all 
 - variables with unique names
-}


fixLits :: [Function] -> CompilerM [Function]
fixLits = functionApply (descendM fl)
    where
    fl :: Expr Var -> CompilerM (Expr Var)
    fl (Lit (Int i)) = return $ App (Var iBoxV) (Lit (Int i))
    fl e = return e

rename :: [Function] -> CompilerM [Function]
rename = functionApply (descendM renameM)

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

renameM (Case b e t alts) = 
    case e of
        v@TyVar{} -> do
            fr <- fresh
            let v' = appendName v "_rn" fr
            let alts' = rp v v' fr alts
            alts'' <- mapM rn alts'
            return $! Case b v' t alts''
        _ -> do
            alts'' <- mapM rn alts
            return $! Case b Hole t alts''
    where
        rp :: Var -> Var -> Int -> [Alt Var] -> [Alt Var]
        rp v v' fr ((ac,b,e):rest) =
            let e' = descend (replaceAllNames (varName v) (varName v') fr) e
            in (ac,b,e'):(rp v v' fr rest)
        rp _ _ _ [] = []
        rn :: Alt Var -> CompilerM (Alt Var)
        rn a@(c,b,e) = do
            b' <- mapM rv b
            let bzipped = zip b b'
            let e' = foldl' (\e (b,(b',fr)) -> 
                                descend (replaceAllNames (varName b) (varName b') fr) e)
                        e bzipped
            return (c,map fst b',e')

        rv :: Var -> CompilerM (Var,Int)
        rv v = do
            fr <- fresh
            return $ (appendName v "_rn" fr,fr)
                
        
renameM x = return x

replaceAllNames :: String -> String -> Int -> Expr Var -> Expr Var
replaceAllNames oldName newName i (Var v@TyVar{}) 
    | varName v == oldName = Var $ v {varName = newName, realUnique = i}
replaceAllNames oldName newName i (Case e v@TyVar{} t a) 
    | varName v == oldName = Case e (v {varName = newName, realUnique = i}) t a
replaceAllNames _ _ _ x =  x

    

appendName :: Var -> String -> Int -> Var
appendName v s i = 
    if '_' `elem` (varName v) then
        let (name,_) = T.breakOnEnd "_" $ T.pack $ varName v
            name' = T.unpack $ T.init $ name
        in v {varName = name' ++ s ++ show i, realUnique = i}
    else
        v {varName = varName v ++ s ++ show i, realUnique = i}


