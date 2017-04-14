{-# Language DeriveDataTypeable, OverloadedStrings, FlexibleInstances #-}
module Core where
import Type
import Weight
import DataCon
import qualified Data.Text as T
import Data.Data
import Data.Functor.Identity


data Expr b 
    = Var Var
    | Lit Literal
    | App (Expr b) (Arg b)
    | Lam b (Expr b)
    | Let (Bind b) (Expr b)
    | Op Binop (Expr b) (Expr b)
    | Case (Expr b) b Type [Alt b]
    | Type Type
    deriving (Show,Data)

data Literal 
    = Int Int
    | Bool Bool
    deriving (Show,Eq,Ord,Data)
data Binop = Add | Sub | Mul
    deriving (Eq, Ord, Show, Data)

data Var 
    = TyVar {
        varName    :: !String,
        realUnique :: !Int,
        varType    :: Type,
        varWeight  :: Weight
    }
    | Hole
    deriving (Eq,Ord,Data)
instance Show Var where
    show v = "(TyVar " ++ varName v ++ ")"
type Arg b = Expr b
type Alt b = (AltCon, [b], Expr b)

data AltCon
    = LitAlt Literal
    {-| DataAlt DataCon-}
    | Default
    deriving (Eq, Data, Show)

data Bind b 
    = NonRec b (Expr b)
    | Rec [(b, Expr b)]
    deriving (Data)
instance Show b => Show (Bind b) where
    show (NonRec v e) = "NonRec (" ++ show v ++ ") (" ++ show e ++ ")"

data Function = Function {
    fName :: String,
    fType :: Type,
    fBody :: Expr Var
    }
    deriving (Show,Data)

mkVar :: Type -> String -> Var
mkVar t s = TyVar { varName=s,realUnique=0,varType=t,varWeight=Omega}

iVar :: String -> Var
iVar = mkVar $ TCon "Int"

bVar :: String -> Var
bVar = mkVar $ TCon "Bool"


class Monad m => FreshMonad m where
    fresh :: m Int
--  freshName takes the variable name and module name and removes
--  the old module name and appends the new module name and a unique number
    freshName :: String -> String -> m String
    freshName str mod = do
        let (pre,_) = T.breakOnEnd "_" (T.pack str) 
        fr <- fresh
        return $ T.unpack pre ++ "_" ++ mod ++ show fr

descendM :: (Monad m, Applicative m) => (Expr b -> m (Expr b)) -> Expr b -> m (Expr b)
descendM f e = 
    let res = case e of
                Var v -> Var <$> pure v
                Lit l -> Lit <$> pure l
                App e1 e2 -> App <$> descendM f e1 <*> descendM f e2 
                Lam b e -> Lam <$> pure b <*> descendM f e
                Let b e -> Let <$> descendBindM f b <*> descendM f e
                Op o e1 e2 -> Op <$> pure o <*> descendM f e1 <*> descendM f e2
                Case e b t alts -> Case <$> descendM f e <*> pure b <*> pure t <*> descendA f alts
                Type t -> Type <$> pure t
    in res >>= f

descendBindM :: (Monad m, Applicative m) => (Expr b -> m (Expr b)) -> Bind b -> m (Bind b)
descendBindM f b = 
    case b of
        NonRec b e -> NonRec <$> pure b <*> descendM f e
        Rec ((b,e):bs) -> do
            e' <- descendM f e
            Rec bs' <- descendBindM f (Rec bs)
            return $ Rec $ (b,e'):bs'

descend :: (Expr b -> Expr b) -> Expr b -> Expr b
descend f ex = runIdentity (descendM (return . f) ex)

descendA :: (Monad m, Applicative m) => (Expr b -> m (Expr b)) -> [Alt b] -> m [Alt b] 
descendA f ((c,b,e):rest) = do
    e' <- f e
    r' <- descendA f rest
    return $ (c,b,e'):r'
descendA _ [] = return []
    

replaceAllVars :: Var -> Expr Var -> Expr Var -> Expr Var
replaceAllVars s r (Var v) | s == v = r
replaceAllVars _ _ e = e

