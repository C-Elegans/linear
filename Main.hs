module Main where
import Core
import Core.Helper
import Core.Passes
import Pretty
import Type
import Flags
import Stg
import Stg.CoreToStg
import Stg.Compile
import Cmm.Cmm
import DataCon
import DefaultTypes
import Data.Maybe (mapMaybe)
import System.Environment
import qualified Data.Set as S

innerExpr = Let (NonRec (iVar "x") (Op Add (Lit (Int 3)) (Lit (Int 4)))) (Op Add (Var (iVar "x")) (Var (iVar "c")))
innerExpr2 = (Op Add (Op Add (Var (iVar "a")) (Var (iVar "b"))) (Var (iVar "c")))
testExpr  = Lam (iVar "a") $ Lam (iVar "b") $ Lam (iVar "c") innerExpr
testExpr2  = Lam (iVar "a") $ Lam (iVar "b") $ Lam (iVar "c") innerExpr2
testExpr3  = Lam (iVar "b") $ Case (Op Add (Var (iVar "b")) (Lit (Int 1))) (iVar "binder") (TCon "Bool")
    [(LitAlt (Int 1), [], Lit (Int 1)), (Default, [iVar "x"], Var (iVar "x"))]
testExpr4  = Lam (iVar "a") $ App (Lam (iVar "b") (Op Add (Var (iVar "b")) (Lit (Int 1)))) (Var (iVar "a"))

addExpr = Lam (iVar "a") $ Lam (iVar "b") $ Case (Var (iVar "a")) Hole tInt
    [(DataAlt iBox,[iVar "a'"], Case (Var (iVar "b")) Hole tInt 
        [(DataAlt iBox,[iVar "b'"], Case (Op Add# (Var (iVar "a'")) (Var (iVar "b'"))) (iVar "r") tIntU [(Default,[],App (Var iBoxV) (Var (iVar "r")))])])]

func = Function {fName="test", fType= tInt `TArr` tInt `TArr` tInt `TArr` tInt, 
    fBody=testExpr, alwaysInline=False}
func2 = Function {fName="test2", fType= tInt `TArr` tInt `TArr` tInt `TArr` tInt, 
    fBody=testExpr2, alwaysInline=False}
func3 = Function {fName="testCase", fType = tInt `TArr` tInt, 
    fBody=testExpr3, alwaysInline=False} 
func4 = Function {fName="testBeta", fType = tInt `TArr` tInt, 
    fBody=testExpr4, alwaysInline=False} 
fadd = Function {fName="add", fType = TyConApp iBoxT [tInt], 
    fBody=addExpr, alwaysInline=True} 
{-funcs = [func,func2,func3,func4,fadd]-}
funcs = [func4,fadd]


main :: IO ()
main = do
    args <- getArgs
    print args
    {-let flags = S.fromList $ mapMaybe flagFor $ concatMap words args-}
    let flags = allFlags
    (result,_) <- runCompilerM (passes funcs) (emptyCs {_flags=flags} )
    case result of
        Right funcs' -> do 
            putStrLn $ banner  "Output"
            mapM_ (putStrLn . pp False) funcs'
            (Right stg,_) <- runCompilerM (coreToStg funcs') (emptyCs)
            putStrLn $ banner "Stg"
            mapM_ (putStrLn . pp False) stg 
            putStrLn $ banner "Compiled"
            putStrLn $ render $ compile stg
        Left err -> putStrLn $ "Error: " ++  err

