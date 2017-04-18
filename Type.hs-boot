{-# Language DeriveDataTypeable #-}
module Type where
import Data.Data
import Weight
type Con = String
type KindOrType = Type
type Kind = Type
data Type

data TVar
data Var 

tInt :: Type

tBool :: Type

tIntU :: Type

iVar :: String -> Var

bVar :: String -> Var

iVarU :: String -> Var

mkVar :: Type -> String -> Var

instance Data Type
instance Eq Type
instance Show Type
instance Ord Type


instance Data TVar
instance Eq TVar
instance Ord TVar
instance Show TVar

instance Data Var
instance Eq Var
instance Ord Var
instance Show Var


