module Core.Inliner where
import Core
import Type
import Core.Helper
import Control.Monad
import Data.Maybe
import Debug.Trace (trace)
import Control.Monad.State

inline :: [Function] -> CompilerM [Function]
inline = functionApply ((descendM opToFunc) >=> (descendM inlineM))

opToFunc :: Expr Var -> CompilerM (Expr Var)
opToFunc e@(Op o l r) = 
    case (getF o) of
        Just v -> return $! App (App (Var v) l ) r
        Nothing -> return e
opToFunc e = return e

inlineM :: Expr Var -> CompilerM (Expr Var)
{-inlineM e@App{} | trace (show e) False = undefined-}
inlineM e@(App (Var n) a) = do
    f <- getFunction (varName n)
    st <- get
    {-inIO $ print f-}
    {-inIO $ print st-}
    if isJust f && alwaysInline (fromJust f) then do
        let body = fBody (fromJust f)
        return $! App (body) a
    else
        return e
inlineM e = return e

getF :: Binop -> Maybe Var
getF Add = Just $ mkVar (tInt `TArr` tInt `TArr` tInt) "add"
getF _ = Nothing
