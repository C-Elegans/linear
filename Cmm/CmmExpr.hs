module Cmm.CmmExpr where
import Cmm.CmmType
import Cmm.CmmMachOp
import Compiler.Hoopl hiding (Unique)
type Unique = Int
type BlockId = Label

data CLabel = CLabel String
    deriving(Eq,Ord,Show)
data CmmExpr 
    = CmmLit CmmLit
    | CmmLoad !CmmExpr !CmmType
    | CmmReg !CmmReg
    | CmmMachOp MachOp [CmmExpr] -- Machine operation (+,-,*,etc.)
    | CmmStackSlot Area !Int
    | CmmRegOff !CmmReg Int

instance Eq CmmExpr where       -- Equality ignores the types
    CmmLit l1          == CmmLit l2          = l1==l2
    CmmLoad e1 _       == CmmLoad e2 _       = e1==e2
    CmmReg r1          == CmmReg r2          = r1==r2
    CmmRegOff r1 i1    == CmmRegOff r2 i2    = r1==r2 && i1==i2
    CmmMachOp op1 es1  == CmmMachOp op2 es2  = op1==op2 && es1==es2
    CmmStackSlot a1 i1 == CmmStackSlot a2 i2 = a1==a2 && i1==i2
    _e1                == _e2                = False

data CmmReg
    = CmmLocal !LocalReg
    | CmmGlobal GlobalReg
    deriving (Eq,Ord)

data Area
    = Old
    | Young !BlockId
    deriving (Eq,Ord)

data CmmLit
    = CmmInt !Integer Width
    {-| CmmFloat Rational Width-}
    | CmmVec [CmmLit]
    | CmmLabel CLabel
    | CmmLabelOff CLabel Int
    | CmmLabelDiffOff CLabel CLabel Int
    | CmmBlock !BlockId
    | CmmHighStackMark
    deriving (Eq)

data LocalReg 
    = LocalReg !Unique CmmType

instance Eq LocalReg where
    (LocalReg u1 _) == (LocalReg u2 _) = u1 == u2

instance Ord LocalReg where
    compare (LocalReg u1 _) (LocalReg u2 _) = u1 `compare` u2

data GlobalReg 
    = VanillaReg !Int VGcPtr
    | LongReg !Int
    | Sp
    | SpLim
    | Hp
    | HpLim
    | CCCs -- current cost center stack
    | CurrentTSO
    | CurrentNursery
    | HpAlloc
    | GcEnter1
    | GcFun
    | BaseReg
    | MachSp -- C stack pointer
    | PicBaseReg
    deriving (Eq,Ord,Show)

data VGcPtr = VGcPtr | VNonGcPtr deriving( Eq, Show, Ord )

node :: GlobalReg
node = VanillaReg 1 VGcPtr

baseReg, spReg, hpReg, spLimReg, nodeReg :: CmmReg
baseReg = CmmGlobal BaseReg
spReg = CmmGlobal Sp
hpReg = CmmGlobal Hp
spLimReg = CmmGlobal SpLim
nodeReg = CmmGlobal node
