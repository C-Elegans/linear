module Main where
import Core
import Core.Passes
import Pretty
import Type

innerExpr = Let (NonRec (iVar "x") (Op Add (Lit (Int 3)) (Lit (Int 4)))) (Op Add (Var (iVar "a")) (Var (iVar "c")))
innerExpr2 = Let (NonRec (iVar "x") (Op Add (Var (iVar "a")) (Var (iVar "b")))) (Op Add (Var (iVar "x")) (Var (iVar "c")))
testExpr  = Lam (iVar "a") $ Lam (iVar "b") $ Lam (iVar "c") innerExpr
testExpr2  = Lam (iVar "a") $ Lam (iVar "b") $ Lam (iVar "c") innerExpr2

func = Function {fName="test", fType= tInt `TArr` tInt `TArr` tInt `TArr` tInt, fBody=testExpr}
func2 = Function {fName="test2", fType= tInt `TArr` tInt `TArr` tInt `TArr` tInt, fBody=testExpr2}
funcs = [func,func2]

main :: IO ()
main = do
    mapM_ (putStrLn . pp True) funcs
    mapM_ (putStrLn . pp False) $ runPasses funcs

