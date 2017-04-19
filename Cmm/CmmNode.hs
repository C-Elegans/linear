{-# Language CPP, GADTs, ExplicitForAll, StandaloneDeriving #-}
module Cmm.CmmNode where
import Data.Maybe (maybeToList)
import Prelude hiding (succ)
import Compiler.Hoopl
import Cmm.CmmExpr
import Cmm.CmmType
import Cmm.CmmMachOp

#define ULabel {-# UNPACK #-} !Label
data ByteOff
data SwitchTargets

data CmmNode e x where
    CmmEntry             :: ULabel        -> CmmNode C O
    CmmComment           :: String        -> CmmNode O O
    CmmAssign            :: !CmmReg       -> !CmmExpr      -> CmmNode O O
    CmmStore             :: !CmmExpr      -> !CmmExpr      -> CmmNode O O
    CmmUnsafeForeignCall :: ForeignTarget -> [CmmFormal]   -> [CmmActual] -> CmmNode O O
    CmmBranch            :: ULabel        -> CmmNode O C
    CmmCondBranch        :: CmmExpr       -> ULabel        -> ULabel      -> CmmNode O C
    CmmSwitch            :: CmmExpr       -> SwitchTargets -> CmmNode O C
    CmmCall              :: 
        {
            cml_target    :: CmmExpr,
            cml_cont      :: Maybe Label, 
                -- Label of continuation
            cml_args_regs :: [GlobalReg], 
                -- Arguments that are passed to the call
            cml_args      :: ByteOff,
            cml_ret_args  :: ByteOff, 
                -- Unknown, see ghc/CmmNode.hs:125 for details
            cml_ret_off   :: ByteOff
        } -> CmmNode O C

    CmmForeignCall :: 
        {
            tgt      :: ForeignTarget,
            res      :: [CmmFormal],
            args     :: [CmmActual],
            succ     :: ULabel,
            ret_args :: ByteOff,
            ret_off  :: ByteOff
        } -> CmmNode O C

data ForeignConvention
    = ForeignConvention 
        CCallConv     -- which convention
        [ForeignHint] -- arg hints
        [ForeignHint] -- return hints
        CmmReturnInfo
    deriving Eq
data CCallConv 
    = CCallConv | CApiConv | StdCallConv | PrimCallConv | JavaScriptCallConv
    deriving (Eq)

data CmmReturnInfo
    = CmmMayReturn
    | CmmNeverReturns
    deriving Eq

data ForeignTarget 
    = ForeignTarget CmmExpr ForeignConvention
    | PrimTarget CallishMachOp

instance NonLocal CmmNode where
    entryLabel (CmmEntry l) = l

    successors (CmmBranch l) = [l]
    successors (CmmCondBranch _ t f) = [f,t]
    successors (CmmSwitch _ ids) = switchTargetsToList ids
    successors (CmmCall {cml_cont=l}) = maybeToList l
    successors (CmmForeignCall {succ=l}) = [l]

type CmmActual = CmmExpr
type CmmFormal = LocalReg
switchTargetsToList :: SwitchTargets -> [Label]
switchTargetsToList _ = []
{-deriving instance Eq (CmmNode e x)-}

