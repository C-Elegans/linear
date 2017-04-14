{-# Language TypeSynonymInstances, FlexibleInstances, GeneralizedNewtypeDeriving #-}
module Core.Helper where
import Flags
import Core
import Control.Monad.State
import Control.Monad.Except
import Control.Applicative
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
    _flags :: Flags
    }
    deriving (Eq,Show)

emptyCs :: CoreState
emptyCs = CoreState {
    _currentUnique = 0,
    _flags = initialFlags
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
