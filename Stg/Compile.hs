module Stg.Compile where
import Prelude hiding ((<$>))
import Stg
import Core
import Core.Helper
import Text.PrettyPrint.Leijen
import Pretty

compile :: [StgTopBinding] -> Doc
compile = vcat . (map compileBind)

compileBind :: StgTopBinding -> Doc
compileBind (StgTopLifted (StgNonRec nVar (StgRhsClosure _ _ args bdy))) = 
    text (varName nVar) <+> ppr 0 False args <+> text "=" <$> nest 4 (compileExpr bdy)

compileExpr :: StgExpr -> Doc
compileExpr (GenStgLet (StgNonRec v c@StgRhsClosure{}) e2) = text "Closure alloc" <+> ppr 0 False c <$> compileExpr e2
compileExpr e = empty
compileExpr (GenStgLet (StgNonRec e11 (StgRhsCon e121 e122)) e2) = text "Heap alloc"
compileExpr (GenStgApp e1 e2) = undefined
compileExpr (GenStgLit e) = undefined
compileExpr (GenStgConApp e1 e2 e3) = undefined
compileExpr (GenStgOpApp e1 e2 e3) = undefined
compileExpr (GenStgLam e1 e2) = undefined
compileExpr (GenStgCase e1 e2 e3 e4) = undefined
