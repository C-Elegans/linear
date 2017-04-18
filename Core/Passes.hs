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
import Core.Atomize
import Core.Inliner

passes :: [Function] -> CompilerM [Function]
passes fs = 
    printPass DumpCore fs                 `bind`

    rename       `bind` printPass DumpRename `bind`
    runSimplify  `bind` printPass DumpSimp   `bind`
    put `bind` 
    inline       `bind` printPass DumpInline `bind`
    runSimplify  `bind` printPass DumpSimp   `bind`
    atomize      `bind` printPass DumpAtom   `bind`
    reduceLinear `bind` printPass DumpLin

printPass :: Flag -> [Function] -> CompilerM [Function]
printPass fl fs = do
    ifSet fl $ do
        inIO $ putStrLn $ banner (show fl) 
        inIO $ mapM_ (putStrLn . pp False)  fs
    return fs

ppIO :: Pretty p => p -> IO ()
ppIO p = 
    let s = pp False p
    in putStrLn s
bind :: CompilerM [Function] -> ([Function] -> CompilerM a) -> CompilerM a
bind fs func = 
    {-fs >>= (mapM putFunction) >>= return fs >>= func-}
    fs >>= func

put :: [Function] -> CompilerM [Function]
put fs = do 
    mapM_ putFunction fs 
    return fs
