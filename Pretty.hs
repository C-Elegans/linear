{-# Language TypeSynonymInstances #-}
{-# Language FlexibleInstances #-}
module Pretty where
import Core
import Type
import Weight
import Text.PrettyPrint
parensIf :: Bool -> Doc -> Doc
parensIf True = parens
parensIf False = id

class Pretty p where
    ppr :: Int -> Bool -> p -> Doc

instance Pretty Var where
    ppr _ tp x@TyVar{varWeight=weight}  = 
        if tp then 
            text (varName x) <> text "∷" <> ppr 0 True (varType x) <> ppr 0 True weight
        else
            text (varName x) <> ppr 0 True weight
    ppr _ _ Hole          = "_"
instance Pretty Weight where
    ppr _ _ Omega = ":ω"
    ppr _ _ One   = ":1"
    ppr _ _ Zero  = ":0"

instance Pretty (Expr Var) where
    ppr p tp (Var a)      = ppr p tp a
    ppr p tp (App a b)    = parensIf (p>0) $ ppr (p+1) tp a <+> ppr p tp b
    ppr p tp (Lam a b)    = text "\\" <> ppr p tp a <+> text "->" <+> ppr p tp b
    ppr p tp (Let a b)    = text "let " <> ppr p tp a <+> text "in" <+> ppr p tp b
    ppr p tp (Type t)     = ppr p tp t
    ppr p tp (Op o e1 e2) = parensIf (p>0) $ ppr p tp e1 <+> ppr p tp o <+> ppr p tp e2
    ppr p tp (Lit l)      = ppr p tp l

instance Pretty Literal where
    ppr _ _ (Int i) = int i
instance Pretty Binop where
    ppr _ _ Add = text "+"
    ppr _ _ Sub = text "-"
    ppr _ _ Mul = text "*"
instance Pretty Type where
    ppr p True (TVar (TV t)) = text t
    ppr p True (TCon c)      = text c
    ppr p True (TArr t1 t2)  = ppr p True t1 <+> text "→" <+> ppr p True t2
    ppr p True (TLArr t1 t2) = ppr p True t1 <+> text "⊸" <+> ppr p True t2
    ppr _ False _            = text ""
instance Pretty (Bind Var) where
    ppr p tp (NonRec b e) = ppr p tp b <+> text "=" <+> ppr p tp e

instance Pretty Function where
    ppr p True f = text (fName f) <+> text "∷" <+> ppr p True (fType f) $$
        text (fName f) <+> "=" <+> ppr p True (fBody f)
    ppr p False f= text (fName f) <+> text "=" <+> ppr p False (fBody f)

pp :: Pretty p => Bool -> p -> String
pp tp p= render $ ppr 0 tp p 
