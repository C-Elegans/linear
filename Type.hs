{-# Language DeriveDataTypeable #-}
module Type where
import Data.Data
import Weight
type Con = String
data Type
    = TVar TVar
    | TCon Con
    | TArr Type Type
    | TLArr Type Type
    deriving (Show, Eq, Ord,Data)

data TVar = TV String
    deriving (Show, Eq, Ord,Data)
data Var 
    = TyVar {
        varName    :: !String,
        realUnique :: !Int,
        varType    :: Type,
        varWeight  :: Weight
    }
    | Hole
    deriving (Eq,Ord,Data,Show)

tInt :: Type
tInt = TCon "Int"

tBool :: Type
tBool = TCon "Bool"

tIntU :: Type
tIntU = TCon "Int#"

iVar :: String -> Var
iVar = mkVar $ TCon "Int"

bVar :: String -> Var
bVar = mkVar $ TCon "Bool"

iVarU :: String -> Var
iVarU = mkVar $ TCon "Int#"

mkVar :: Type -> String -> Var
mkVar t s = TyVar { varName=s,realUnique=0,varType=t,varWeight=Omega}
