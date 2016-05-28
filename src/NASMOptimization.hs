module NASMOptimization where

import Data.Graph
import Data.Map (fromList, Map(..))
import Debug.Trace (traceShow)

import TACGenerator


constructLabelKey :: [TACInstruction] -> Map TACInstruction Int
constructLabelKey is = 
    let isLabel (TACLabel _, _) = True
        isLabel (_, _) = False
    in fromList $ filter isLabel (zip is [0..])

controlFlowGraph :: [TACInstruction] -> Graph
controlFlowGraph is = 
    let labelKeys = constructLabelKey is
    in traceShow labelKeys undefined