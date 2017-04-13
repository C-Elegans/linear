module Main where
import Core
import Core.Renamer
import Pretty
import Type

innerExpr = Let (NonRec (iVar "x") (Op Add (Var (iVar "a")) (Var (iVar "b")))) (Op Add (Var (iVar "x")) (Var (iVar "c")))
testExpr  = Lam (iVar "a") $ Lam (iVar "b") $ Lam (iVar "c") innerExpr


main :: IO ()
main = do
    putStrLn $ pp true testExpr
    putStrLn $ pp False $ rename testExpr

