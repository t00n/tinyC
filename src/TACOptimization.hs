module TACOptimization (tacOptimize, removeUselessCopy) where

import TACProgram
import TACAnalysis
import Debug.Trace (traceShow)
import Data.List ((\\))
import qualified Graph as G

mapData :: (TACInstruction -> TACInstruction) -> TACProgram -> TACProgram
mapData f (TACProgram staticdata code) = TACProgram (map f staticdata) code

mapCode :: (TACFunction -> TACFunction) -> TACProgram -> TACProgram
mapCode f (TACProgram staticdata code) = TACProgram staticdata (map f code)

filterData :: (TACInstruction -> Bool) -> TACProgram -> TACProgram
filterData f (TACProgram staticdata code) = TACProgram (filter f staticdata) code

tacOptimize :: TACProgram -> TACProgram
tacOptimize = mapCode (mappair removeUselessCopy) . removeUnusedGlobalVariables

-- remove useless copies

removeUselessCopy :: TACInstruction -> TACInstruction -> (TACInstruction, Bool)
removeUselessCopy prev curr@(TACCopy a (TACVar b)) = 
    case prev of 
        (TACBinary var e1 op e2) -> if var == b then (TACBinary a e1 op e2, False) else (prev, True)
        (TACAddress var addr) -> if var == b then (TACAddress a addr, False) else (prev, True)
        _ -> (prev, True)
removeUselessCopy prev _ = (prev, True)

mappair :: Show a => (a -> a -> (a, Bool)) -> [a] -> [a]
mappair _ [] = []
mappair _ [x] = [x]
mappair f (x:y:xs) = let (val, continue) = f x y 
                         next = if continue then y:xs else xs
                     in  val : mappair f next

-- remove unused global variables

removeUnusedGlobalVariables :: TACProgram -> TACProgram
removeUnusedGlobalVariables prog@(TACProgram staticdata funcs) = 
    let globalVariables = retrieveVariables staticdata
        localVariables = concatMap f funcs
        f = G.values . registerInterferenceGraph . dataFlowGraph . controlFlowGraph
        removeVariable (TACCopy a _) = a `notElem` (globalVariables \\ localVariables)
        removeVariable (TACArrayDecl a _) = a `notElem` (globalVariables \\ localVariables)
        removeVariable _ = True
    in  filterData removeVariable prog
