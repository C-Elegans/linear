module Core.Passes(
    passes,
    printPass,
    ppIO
    ) where
import Core
import Core.Helper
import Flags
import Pretty 
import Core.Renamer
import Core.Simplify
import Core.ReduceLinear

passes :: [Function] -> CompilerM [Function]
passes fs = 

    return fs    >>= printPass DumpCore   >>=
    rename       >>= printPass DumpRename >>=
    runSimplify  >>= printPass DumpSimp   >>=
    reduceLinear >>= printPass DumpLin

printPass :: Flag -> [Function] -> CompilerM [Function]
printPass fl fs = do
    ifSet fl $ do
        inIO $ putStrLn $ banner $ show fl
        inIO $ mapM_ (putStrLn . pp False)  fs
    return fs

ppIO :: Pretty p => p -> IO ()
ppIO = putStrLn . pp False
bind = flip (.)
