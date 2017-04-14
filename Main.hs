module Main where
import Core
import Core.Helper
import Core.Passes
import Pretty
import Type
import Flags
import qualified Data.Set as S

innerExpr = Let (NonRec (iVar "x") (Op Add (Lit (Int 3)) (Lit (Int 4)))) (Op Add (Var (iVar "x")) (Var (iVar "c")))
innerExpr2 = Let (NonRec (iVar "x") (Op Add (Var (iVar "a")) (Var (iVar "b")))) (Op Add (Var (iVar "x")) (Var (iVar "c")))
testExpr  = Lam (iVar "a") $ Lam (iVar "b") $ Lam (iVar "c") innerExpr
testExpr2  = Lam (iVar "a") $ Lam (iVar "b") $ Lam (iVar "c") innerExpr2
testExpr3  = Lam (iVar "b") $ Case (Op Add (Var (iVar "b")) (Lit (Int 1))) Hole (TCon "Bool")
    [(LitAlt (Int 1), [], Lit (Int 1)), (Default, [], Lit (Int 0))]
testExpr4  = Lam (iVar "a") $ App (Lam (iVar "b") (Op Add (Var (iVar "b")) (Lit (Int 1)))) (Var (iVar "a"))

func = Function {fName="test", fType= tInt `TArr` tInt `TArr` tInt `TArr` tInt, fBody=testExpr}
func2 = Function {fName="test2", fType= tInt `TArr` tInt `TArr` tInt `TArr` tInt, fBody=testExpr2}
func3 = Function {fName="testCase", fType = tBool `TArr` tInt, fBody=testExpr3} 
func4 = Function {fName="testBeta", fType = tInt `TArr` tInt, fBody=testExpr4} 
funcs = [func,func2,func3,func4]


main :: IO ()
main = do
    let flags = S.fromList [DumpRename, DumpSimp,DumpCore,DumpLin,DumpAtom]
    (result,_) <- runCompilerM (passes funcs) (emptyCs {_flags=flags} )
    case result of
        Right funcs' -> do 
            putStrLn $ banner  "Output"
            mapM_ (putStrLn . pp False) funcs'
        Left err -> putStrLn $ "Error: " ++  err

