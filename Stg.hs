{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Stg where
import Prelude hiding ((<$>))
import Core
import Type
import DataCon
import Pretty
import Text.PrettyPrint.Leijen hiding (Pretty)

data GenStgExpr bndr occ 
    = GenStgApp occ [GenStgArg occ]
    | GenStgLit Literal
    | GenStgConApp DataCon [GenStgArg occ] [Type]
    | GenStgOpApp Binop [GenStgArg occ] Type
    | GenStgLam [bndr] (GenStgExpr bndr occ)
    | GenStgCase (GenStgExpr bndr occ) bndr AltType [GenStgAlt bndr occ]
    | GenStgLet (GenStgBinding bndr occ) (GenStgExpr bndr occ)
    deriving (Eq,Ord,Show)

data GenStgArg occ
    = StgVarArg occ
    | StgLitArg Literal
    deriving(Eq,Ord,Show)
    
data GenStgBinding bndr occ
    = StgNonRec bndr (GenStgRhs bndr occ)
    | StgRec  [(bndr, GenStgRhs bndr occ)]
    deriving(Eq,Ord,Show)

data StgBinderInfo 
    = NoStgBinderInfo
    | SatCallsOnly
    deriving(Eq,Ord,Show)
data GenStgRhs bndr occ
    = StgRhsClosure
        StgBinderInfo         -- info on how the binder is used
        [occ]                 -- free variables
        [bndr]                -- arguments
        (GenStgExpr bndr occ) -- body
    | StgRhsCon
        DataCon
        [GenStgArg occ]
    deriving(Eq,Ord,Show)

type GenStgAlt bndr occ =
    (AltCon,             -- constructor
    [bndr],              -- constructor parameters
    GenStgExpr bndr occ) -- rhs

data AltType 
    = PolyAlt
    | MultiValAlt Int
    {-| AlgAlt TyCon-}
    | PrimAlt
    deriving(Eq,Ord,Show)

data GenStgTopBinding bndr occ
    = StgTopLifted (GenStgBinding bndr occ)
    {-| StgTopStringLit bndr ByteString-}

type StgTopBinding = GenStgTopBinding Var Var
type StgBinding    = GenStgBinding Var Var
type StgArg        = GenStgArg Var
type StgExpr       = GenStgExpr Var Var
type StgRhs        = GenStgRhs Var Var
type StgAlt        = GenStgAlt Var Var

instance Pretty StgExpr where
    ppr p t (GenStgApp e1 e2) = parens (ppr p t e1) <+> hsep (map (ppr p t) e2)
    ppr p t (GenStgLit l) = ppr p t l
    ppr p t (GenStgConApp e1 e2 e3) = error ""
    ppr p t (GenStgOpApp o [e1,e2] ty) = ppr p t e1 <+> ppr p t o <+> ppr p t e2
    ppr p t (GenStgLam e1 e2) = text "\\" <+> hsep (map (ppr p t) e1) <+> text "->" <+> ppr p t e2
    ppr p t (GenStgCase expr bndr typ alts) = 
        text "case" <+> ppr p t expr <+> text "of" <+> ppr p t bndr <$>
            vcat (map (nest 4 . ppr p t) alts)
    ppr p t (GenStgLet e1 e2) = text "let" <+> ppr p t e1 <$> text "in" <+> ppr p t e2

instance Pretty StgArg where
    ppr p t (StgVarArg a) = ppr p t a
    ppr p t (StgLitArg a) = ppr p t a 

instance Pretty StgTopBinding where
    ppr p t (StgTopLifted (StgNonRec b1 b2)) = ppr p t b1 <+> text "=" <$> nest 4 (ppr p t b2)
instance Pretty StgBinding where
    ppr p t (StgNonRec b1 b2) = ppr p t b1 <+> text "=" <+> ppr p t b2

instance Pretty StgRhs where
    ppr p t (StgRhsClosure r1 r2 r3 r4) = text "\\" <> hsep (map (ppr p t) r3) <+>
        brackets (ppr p t r2) <+> text "->" <$> indent 4 (ppr p t r4)
        
    ppr p t (StgRhsCon r1 r2) = error ""
instance Pretty a => Pretty [a] where
    ppr p t as = hsep (map (ppr p t) as)

instance Pretty StgAlt where
    ppr p t (con,binds,expr) = 
        ppr p t con <+> hsep (map (ppr p t) binds) <+> text "->" <+> ppr p t expr
