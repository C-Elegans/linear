module Cmm.CmmType where

data Width = W8 | W16 |W32 | W64
    deriving (Eq,Ord,Show)

data CmmType = CmmType CmmCat Width
    deriving (Eq,Show)

data CmmCat --Category
    = GcPtrCat
    | PtrCat 
    | BitsCat
    deriving (Eq,Show)

data ForeignHint 
    = NoHint | AddrHint | SignedHint
    deriving Eq
