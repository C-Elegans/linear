{-# Language DeriveDataTypeable #-}
module Type where
import Data.Data
type Con = String
data Type
    = TVar TVar
    | TCon Con
    | TArr Type Type
    | TLArr Type Type
    deriving (Show, Eq, Ord,Data)

data TVar = TV String
    deriving (Show, Eq, Ord,Data)

tInt :: Type
tInt = TCon "Int"
