{-# Language TypeSynonymInstances #-}
{-# Language FlexibleInstances #-}
module Pretty where
import Core
import Type
import Text.PrettyPrint
parensIf :: Bool -> Doc -> Doc
parensIf True = parens
parensIf False = id

class Pretty p where
    ppr :: Int -> p -> Doc

instance Pretty Var where
    ppr _ x = text (varName x) <> text "::" <> ppr 0 (varType x)

instance Pretty (Expr Var) where
    ppr p (Var a) = ppr p a
    ppr p (App a b) = parensIf (p>0) $ ppr (p+1) a <+> ppr p b
    ppr p (Lam a b) = text "\\" <> ppr p a <+> text "->" <+> ppr p b
    ppr p (Let a b) = text "let " <> ppr p a <+> text "in" <+> ppr p b
    ppr p (Type t) = ppr p t
    ppr p (Op o e1 e2) = parensIf (p>0) $ ppr p e1 <+> ppr p o <+> ppr p e2
    ppr p (Lit l) = ppr p l

instance Pretty Literal where
    ppr _ (Int i) = int i
instance Pretty Binop where
    ppr _ Add = text "+"
    ppr _ Sub = text "-"
    ppr _ Mul = text "*"
instance Pretty Type where
    ppr p (TVar (TV t)) = text t
    ppr p (TCon c) = text c
    ppr p (TArr t1 t2) = ppr p t1 <+> text " -> " <+> ppr p t2
    ppr p (TLArr t1 t2) = ppr p t1 <+> text " ⊸ " <+> ppr p t2
instance Pretty (Bind Var) where
    ppr p (NonRec b e) = ppr p b <+> text "=" <+> ppr p e
pp :: Pretty p => p -> String
pp = render . ppr 0
