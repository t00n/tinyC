module TACOptimization (tacOptimize, removeUselessCopy) where

import TACGenerator
import TACAnalysis
import Debug.Trace (traceShow)

tacOptimize :: TACProgram -> TACProgram
tacOptimize (TACProgram staticdata code) = TACProgram staticdata $ map optimizeFunction code

optimizeFunction :: TACFunction -> TACFunction
optimizeFunction = mappair removeUselessCopy

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
