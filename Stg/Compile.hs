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
    text (varName nVar) <+> ppr 0 False args <+> text "=" <$> indent 4 (compileExpr bdy)

compileExpr :: StgExpr -> Doc
compileExpr (GenStgLet (StgNonRec v c@StgRhsClosure{}) e2) = text "Closure alloc" <+> ppr 0 False c <$> compileExpr e2
compileExpr (GenStgCase e bind e3 [(Default,[],a)]) = 
    text "let!" <+> ppr 0 False bind <+> text "=" <+> ppr 0 False e <$>
    text "in" <+> ppr 0 False a
compileExpr (GenStgCase e e2 e3 args) = text "Eval" <+> ppr 0 False e <$>
    indent 4 (vcat (map compileArg args))
compileExpr e = empty
compileExpr (GenStgLet (StgNonRec e11 (StgRhsCon e121 e122)) e2) = text "Heap alloc"
compileExpr (GenStgApp e1 e2) = undefined
compileExpr (GenStgLit e) = undefined
compileExpr (GenStgConApp e1 e2 e3) = undefined
compileExpr (GenStgOpApp e1 e2 e3) = undefined
compileExpr (GenStgLam e1 e2) = undefined

compileArg :: StgAlt -> Doc
compileArg (con,binds,expr) = ppr 0 False con <+> hsep (map (ppr 0 False) binds) <+> text "->" <+> compileExpr expr
