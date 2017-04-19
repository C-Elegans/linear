module Cmm.CmmMachOp where
import Cmm.CmmType

data MachOp
    = MO_Add Width
    | MO_Sub Width
    | MO_Eq Width
    | MO_NE Width
    | MO_Mul Width

    -- Signed comparisons
    | MO_S_Ge Width
    | MO_S_Le Width
    | MO_S_Gt Width
    | MO_S_Lt Width

    -- Unsigned comparisons
    | MO_U_Ge Width
    | MO_U_Le Width
    | MO_U_Gt Width
    | MO_U_Lt Width

    -- Bitwise Ops
    | MO_And   Width
    | MO_Or    Width
    | MO_Xor   Width
    | MO_Not   Width
    | MO_Shl   Width
    | MO_U_Shr Width      -- unsigned shift right
    | MO_S_Shr Width      -- signed shift right
    deriving (Eq,Show)

data CallishMachOp
    = MO_F64_Pwr
    | MO_F64_Sin
    | MO_F64_Cos
    | MO_F64_Tan
    | MO_F64_Sinh
    | MO_F64_Cosh
    | MO_F64_Tanh
    | MO_F64_Asin
    | MO_F64_Acos
    | MO_F64_Atan
    | MO_F64_Log
    | MO_F64_Exp
    | MO_F64_Fabs
    | MO_F64_Sqrt
    | MO_F32_Pwr
    | MO_F32_Sin
    | MO_F32_Cos
    | MO_F32_Tan
    | MO_F32_Sinh
    | MO_F32_Cosh
    | MO_F32_Tanh
    | MO_F32_Asin
    | MO_F32_Acos
    | MO_F32_Atan
    | MO_F32_Log
    | MO_F32_Exp
    | MO_F32_Fabs
    | MO_F32_Sqrt

    | MO_UF_Conv Width

    | MO_S_QuotRem Width
    | MO_U_QuotRem Width
    | MO_U_QuotRem2 Width
    | MO_Add2      Width
    | MO_SubWordC  Width
    | MO_AddIntC   Width
    | MO_SubIntC   Width
    | MO_U_Mul2    Width

    | MO_WriteBarrier
    | MO_Touch         -- Keep variables live (when using interior pointers)

    -- Prefetch
    | MO_Prefetch_Data Int -- Prefetch hint. May change program performance but not
                       -- program behavior.
                       -- the Int can be 0-3. Needs to be known at compile time
                       -- to interact with code generation correctly.
                       --  TODO: add support for prefetch WRITES,
                       --  currently only exposes prefetch reads, which
                       -- would the majority of use cases in ghc anyways


    -- These three MachOps are parameterised by the known alignment
    -- of the destination and source (for memcpy/memmove) pointers.
    -- This information may be used for optimisation in backends.
    | MO_Memcpy Int
    | MO_Memset Int
    | MO_Memmove Int

    | MO_PopCnt Width
    | MO_Clz Width
    | MO_Ctz Width

    | MO_BSwap Width

    -- Atomic read-modify-write.
    | MO_AtomicRMW Width AtomicMachOp
    | MO_AtomicRead Width
    | MO_AtomicWrite Width
    | MO_Cmpxchg Width
    deriving (Eq, Show)

data AtomicMachOp 
    = AMO_Add
    | AMO_Sub
    | AMO_And
    | AMO_OR
    | AMO_Xor
    deriving (Eq,Show)
