{-# Language DeriveDataTypeable #-}
module Core where
import Type
import Data.Data

data Expr b 
    = Var Var
    | Lit Literal
    | App (Expr b) (Arg b)
    | Lam b (Expr b)
    | Let (Bind b) (Expr b)
    | Op Binop (Expr b) (Expr b)
    {-| Case (Expr b) b Type [Alt b]-}
    | Type Type
    deriving (Show,Data)

data Literal 
    = Int Int
    | Bool Bool
    deriving (Show,Eq,Ord,Data)
data Binop = Add | Sub | Mul
    deriving (Eq, Ord, Show, Data)

data Var 
    = TyVar {
        varName    :: !String,
        realUnique :: !Int,
        varType    :: Type
    }
    deriving (Show,Eq,Ord,Data)
type Arg b = Expr b
{-
type Alt b = (AltCon, [b], Expr b)

data AltCon
    = DataAlt DataCon
    | LitAlt Literal
    | Default
    deriving (Eq, Data)
-}
data Bind b 
    = NonRec b (Expr b)
    | Rec [(b, Expr b)]
    deriving (Data)
instance Show (Bind b) where
    show (NonRec _ e) = "Bind "

mkVar :: String -> Type -> Var
mkVar s t = TyVar { varName=s,realUnique=0,varType=t}

iVar :: String -> Var
iVar s = TyVar { varName=s,realUnique=0,varType=TCon "Int"}
