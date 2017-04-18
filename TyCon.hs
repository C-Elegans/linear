{-# Language DeriveDataTypeable #-}
module TyCon where
import Type
import Data.Data
import DataCon
type Kind = Type
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

data AlgTyConRhs
    = AbstractTyCon -- No data for the rhs.
    | DataTyCon {
        data_cons :: [DataCon]
    }
type TyVar = Var
type TyConBinder = TyVarBndr TyVar TyConBndrVis
data TyVarBndr tyvar argf = TvBndr tyvar argf
    deriving(Data)

data TyConBndrVis
    = NamedTCB ArgFlag
    | AnonTCB

data ArgFlag = Required | Specified | Inferred
    deriving (Eq, Data)


