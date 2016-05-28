module TACAnalysis where

import Data.Set (Set(..), insert, empty)
import Data.Map (fromList, toList, Map(..), (!))
import qualified Queue as Q
import Debug.Trace (traceShow)

import TACGenerator

data Graph a = Graph (Set (Node a)) (Set Edge)
    deriving (Eq, Show)

type Node a = (Int, a)

type Edge = (Int, Int)

addNode :: Ord a => Node a -> Graph a -> Graph a
addNode x (Graph nodes edges) = Graph (insert x nodes) edges

addEdge :: Ord a => Int -> Int -> Graph a -> Graph a
addEdge p c (Graph nodes edges) = Graph nodes (insert (p, c) edges)

emptyGraph :: Graph a
emptyGraph = Graph empty empty


constructLabelKey :: [TACInstruction] -> Map String Int
constructLabelKey is = 
    let isLabel (TACLabel _, _) = True
        isLabel (_, _) = False
    in fromList $ map (\(TACLabel x, i) -> (x, i)) $ filter isLabel (zip is [0..])

controlFlowGraph2 :: [TACInstruction] -> Int -> Map String Int -> Graph TACInstruction -> Graph TACInstruction
controlFlowGraph2 [] _ _ g = g
controlFlowGraph2 (x:xs) i labels g =
    let graphplusnode = addNode (i, x) g
        graphplusedges = case x of
                        TACIf _ s -> ((addEdge i (labels ! s)) . (addEdge i (i+1))) graphplusnode
                        TACGoto s -> addEdge i (labels ! s) graphplusnode
                        TACCall s _ -> addEdge i (labels ! s) graphplusnode
                        TACReturn _ -> graphplusnode
                        _ -> addEdge i (i+1) graphplusnode
    in controlFlowGraph2 xs (i+1) labels graphplusedges

controlFlowGraph :: [TACInstruction] -> Graph TACInstruction
controlFlowGraph is = controlFlowGraph2 is 0 (constructLabelKey is) emptyGraph


dataFlow :: Graph TACInstruction -> [(Set String, Set String)]
dataFlow (Graph nodes edges) = 
    let invar = fromList [(i, empty :: Set String) | i <- [0..(length nodes) - 1]]
        outvar = fromList [(i, empty :: Set String) | i <- [0..(length nodes) - 1]]
        toProcess = Q.enqueueAll [0..(length nodes) - 1] Q.empty
    in zip (map snd (toList invar)) (map snd (toList outvar))