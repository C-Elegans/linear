module Core.Passes(
    runPasses
    ) where
import Core
import Core.Renamer
import Core.Simplify
import Core.ReduceLinear

runPasses :: [Function] -> [Function]
runPasses = 
    rename              `bind`
    runSimplify         `bind`
    reduceLinear

bind = flip (.)
