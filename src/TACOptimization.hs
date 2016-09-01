module TACOptimization (tacOptimize, removeUselessCopy) where

import TACProgram
import TACAnalysis
import Debug.Trace (traceShow, trace)
import Data.List ((\\))
import qualified Graph as G
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Arrow ((***))

mapData :: (TACInstruction -> TACInstruction) -> TACProgram -> TACProgram
mapData f (TACProgram staticdata code) = TACProgram (map f staticdata) code

mapCode :: (TACFunction -> TACFunction) -> TACProgram -> TACProgram
mapCode f (TACProgram staticdata code) = TACProgram staticdata (map f code)

filterData :: (TACInstruction -> Bool) -> TACProgram -> TACProgram
filterData f (TACProgram staticdata code) = TACProgram (filter f staticdata) code

tacOptimize :: TACProgram -> TACProgram
tacOptimize = replaceCommonExpressions . mapCode (mappair removeUselessCopy) . replaceConstants . removeUnusedGlobalVariables

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

-- replace common expressions

replaceCommonExpressions' :: TACFunction -> TACFunction
replaceCommonExpressions' xs = map replace (zip [0..] xs)
    where efg = (expressionFlowGraph . controlFlowGraph) xs
          replace (i, inst) = newinst
                where (expr, (lhs, _)) = genExpressions inst
                      (flowin, _) = efg M.! i
                      newinst = case expr of
                                     Nothing -> inst
                                     (Just e) -> case M.lookup e flowin of
                                                      Nothing -> inst
                                                      (Just (rhs, _)) -> TACCopy lhs (TACVar rhs)


replaceCommonExpressions :: TACProgram -> TACProgram
replaceCommonExpressions (TACProgram staticdata code) = TACProgram staticdata (map replaceCommonExpressions' code) 

-- replace constants

replaceConstants' :: TACFunction -> TACFunction
replaceConstants' xs = map f (zip [0..] xs)
    where efg = (M.map (M.filterWithKey isConstant *** M.filterWithKey isConstant) . expressionFlowGraph . controlFlowGraph) xs
          isConstant (TACInt _) _ = True
          isConstant (TACChar _) _ = True
          isConstant _ _ = False
          f (i, inst) = newinst
                where (expr, (lhs, used)) = genExpressions inst
                      (flowin, _) = efg M.! i
                      es = M.filterWithKey (\k (def, _) -> def `S.member` used) flowin
                      invertedES = (M.fromList . M.elems . M.mapWithKey (\k (def, _) -> (def, k))) es
                      newinst = case expr of 
                                     Nothing -> inst
                                     (Just e) -> if S.size used <= M.size es then
                                                    replaceInst inst
                                                 else inst
                      replaceInst (TACBinary v (TACVar v1) op (TACVar v2)) = TACBinary v (invertedES M.! v1) op (invertedES M.! v2)
                      replaceInst (TACBinary v (TACVar v1) op e2) = TACBinary v (invertedES M.! v1) op e2
                      replaceInst (TACBinary v e1 op (TACVar v2)) = TACBinary v e1 op (invertedES M.! v2)
                      replaceInst inst = inst

replaceConstants :: TACProgram -> TACProgram
replaceConstants (TACProgram staticdata code) = TACProgram staticdata (map replaceConstants' code)