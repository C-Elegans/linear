{-# Language DeriveDataTypeable #-}
module TyCon where
import {-# SOURCE #-} Type
import Data.Data
import DataCon
type Arity = Int
type TyConRepName = String

data TyCon
    = FunTyCon { --function type constructor
        tyConUnique  :: !Int,
        tyConName    :: String,
        tyConBinders :: [TyConBinder],
        tyConResKind :: Kind,
        tyConKind    :: Kind,
        tyConArity   :: Arity,
        tcRepName    :: TyConRepName
    }
    | AlgTyCon {
        tyConUnique   :: !Int,
        tyConName     :: String,
        tyConBinders  :: [TyConBinder],
        tyConTyVars   :: [TVar],
        tyConResKind  :: Kind,
        tyConKind     :: Kind,
        tyConArity    :: Arity,
        tcRepName     :: TyConRepName,
        {-tcRoles     :: [Role],-}
        {-tyConCType  :: Maybe CType,-}
        algTcRhs      :: AlgTyConRhs
        {-algTcFields   :: FieldLabelEnv-}
        {-algTcParent :: AlgTyConFlav-}
    }
    deriving (Data,Show,Eq,Ord)

data AlgTyConRhs
    = AbstractTyCon -- No data for the rhs.
    | DataTyCon {
        data_cons :: [DataCon]
    }
    deriving (Data,Show,Eq,Ord)
type TyVar = Var
type TyConBinder = TyVarBndr TyVar TyConBndrVis
data TyVarBndr tyvar argf = TvBndr tyvar argf
    deriving(Data,Show,Eq,Ord)

data TyConBndrVis
    = NamedTCB ArgFlag
    | AnonTCB
    deriving (Data,Show,Eq,Ord)

data ArgFlag = Required | Specified | Inferred
    deriving (Eq, Data,Show,Ord)


