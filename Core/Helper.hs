{-# Language TypeSynonymInstances, FlexibleInstances, GeneralizedNewtypeDeriving #-}
module Core.Helper where
import Flags
import Core
import Control.Monad.State
import Control.Monad.Except
import Control.Applicative
import qualified Data.Map as Map
import Data.Monoid

type CompilerMonad = ExceptT Msg (StateT CoreState IO)
newtype CompilerM a = Compiler {runCompiler :: CompilerMonad a}
    deriving
    ( Functor
    , Applicative
    , Alternative
    , Monad
    , MonadFix
    , MonadPlus
    , MonadIO
    , MonadState CoreState
    , MonadError Msg
    ) 

data CoreState = CoreState {
    _currentUnique :: Int,
    _flags :: Flags,
    _topLevelDefs :: Map.Map String Function
    }
    deriving (Eq,Show)

emptyCs :: CoreState
emptyCs = CoreState {
    _currentUnique = 0,
    _flags = initialFlags,
    _topLevelDefs = Map.empty
    }

type Msg = String

instance FreshMonad CompilerM where
    fresh = do
        fr <- gets _currentUnique
        modify' (\s -> s{_currentUnique = _currentUnique s + 1})
        return fr

runCompilerM :: CompilerM a -> CoreState -> IO (Either Msg a, CoreState)
runCompilerM = runStateT . runExceptT . runCompiler

inIO :: IO a -> CompilerM a
inIO = Compiler . liftIO

ifSet :: Flag -> CompilerM a -> CompilerM ()
ifSet flag m = do
    flags <- gets _flags
    when (isSet flags flag) (void m)

putFunction :: Function -> CompilerM ()
putFunction f = do
    let name = fName f
    modify' (\s -> s{_topLevelDefs = Map.insert name f (_topLevelDefs s)})

getFunction :: String -> CompilerM (Maybe Function)
getFunction n = do
    st <- get
    return $ Map.lookup n (_topLevelDefs st)
