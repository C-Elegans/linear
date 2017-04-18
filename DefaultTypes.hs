module DefaultTypes where
import Type
import TyCon
import DataCon


iBoxT :: TyCon
iBoxT = AlgTyCon {
    tyConUnique = 1,
    tyConName = "I#",
    tyConBinders = [],
    tyConTyVars = [],
    tyConResKind = tInt,
    tyConKind = tInt `TArr` tIntU,
    tyConArity = 1,
    tcRepName = "Int",
    algTcRhs = DataTyCon {data_cons=[iBox]}
    }

iBox :: DataCon
iBox = MkData {dcName="I#", dcUnique=0, dcOrigArgTys=[tInt], dcOrigResTy=tIntU}
iBoxV :: Var
iBoxV = mkVar (TyConApp iBoxT [tIntU]) "I#"
