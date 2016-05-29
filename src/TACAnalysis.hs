module TACAnalysis where

import qualified Data.Set as S
import qualified Data.Map as M
import qualified Queue as Q
import Debug.Trace (traceShow, trace)

import TACGenerator

data Graph a = Graph (S.Set Node) (S.Set Edge) (M.Map Node a)
    deriving (Eq, Show)

type Node = Int

type Edge = (Int, Int)

lookupNode :: Ord a => Node -> Graph a -> Maybe a
lookupNode i (Graph _ _ values) = M.lookup i values

addNode :: Ord a => Node -> a -> Graph a -> Graph a
addNode x v (Graph nodes edges values) = Graph (S.insert x nodes) edges (M.insert x v values)

addEdge :: Ord a => Int -> Int -> Graph a -> Graph a
addEdge p c (Graph nodes edges values) = Graph nodes (S.insert (p, c) edges) values

emptyGraph :: Graph a
emptyGraph = Graph S.empty S.empty M.empty

predecessors :: Ord a => Node -> Graph a -> [Node]
predecessors n (Graph _ edges _) = map (\(from, _) -> from) $ filter (\(_, to) -> to == n) $ S.toList edges

successors :: Ord a => Node -> Graph a -> [Node]
successors n (Graph _ edges _) = map (\(_, to) -> to) $ filter (\(from, _) -> from == n) $ S.toList edges

subgraph :: Ord a => [a] -> Graph a -> Graph a
subgraph xs (Graph nodes edges values) = 
    let subGraphValues = M.filter (`elem` xs) values
        subGraphNodes = S.filter (`M.member` subGraphValues) nodes
        subGraphEdges = S.filter (\(p, c) -> S.member p subGraphNodes && S.member c subGraphNodes) edges
    in Graph subGraphNodes subGraphEdges subGraphValues

constructLabelKey :: [TACInstruction] -> M.Map String Int
constructLabelKey is = 
    let isLabel (TACLabel _, _) = True
        isLabel (_, _) = False
    in M.fromList $ map (\(TACLabel x, i) -> (x, i)) $ filter isLabel (zip is [0..])

controlFlowGraph2 :: [TACInstruction] -> Int -> M.Map String Int -> Graph TACInstruction -> Graph TACInstruction
controlFlowGraph2 [] _ _ g = g
controlFlowGraph2 (x:xs) i labels g =
    let graphplusnode = addNode i x g
        graphplusedges = case x of
                        TACIf _ s -> ((addEdge i (labels M.! s)) . (addEdge i (i+1))) graphplusnode
                        TACGoto s -> addEdge i (labels M.! s) graphplusnode
                        TACCall s _ -> addEdge i (labels M.! s) graphplusnode
                        TACReturn _ -> graphplusnode
                        _ -> addEdge i (i+1) graphplusnode
    in controlFlowGraph2 xs (i+1) labels graphplusedges

controlFlowGraph :: [TACInstruction] -> Graph TACInstruction
controlFlowGraph is = controlFlowGraph2 is 0 (constructLabelKey is) emptyGraph


useDefInst :: TACInstruction -> (S.Set String, S.Set String)
useDefInst inst = 
    let
        isVariable (TACVar _) = True
        isVariable (TACArray _ _) = True
        isVariable _ = False
        variableName (TACVar n) = n
        variableName (TACArray n _) = n
        expressionsToSet es = S.fromList $ map variableName $ filter isVariable es
    in
    case inst of 
        (TACBinary v e1 _ e2) -> (expressionsToSet [e1, e2], S.fromList [v])
        (TACUnary s _ e) -> (expressionsToSet [e], S.fromList [s])
        (TACCopy s e) -> (expressionsToSet [e], S.fromList [s])
        (TACArrayAccess s e) -> (expressionsToSet [e], S.fromList[s])
        (TACArrayModif e1 e2) -> (expressionsToSet [e1], expressionsToSet [e2])
        (TACIf e _) -> (expressionsToSet [e], S.empty)
        (TACCall _ es) -> (expressionsToSet es, S.empty)
        (TACReturn Nothing) -> (S.empty, S.empty)
        (TACReturn (Just e)) -> (expressionsToSet [e], S.empty)
        (TACWrite e) -> (expressionsToSet [e], S.empty)
        (TACRead e) -> (S.empty, expressionsToSet [e])
        _ -> (S.empty, S.empty)

dataFlow :: Graph TACInstruction -> M.Map Int (S.Set String, S.Set String)
dataFlow g@(Graph nodes edges values) = 
    let variables = M.fromSet (\k -> (S.empty :: S.Set String, S.empty :: S.Set String)) nodes
        dataFlowRec q vs
            | null q = vs
            | otherwise = 
                let (Just e, newq) = Q.dequeue q
                    succs = M.filterWithKey (\k _ -> k `elem` successors e g) vs
                    out = S.unions $ M.elems $ M.map (\(invar, outvar) -> invar) succs
                    (Just inst) = lookupNode e g
                    (use, def) = useDefInst inst
                    newin = S.union use (S.difference out def)
                    oldin = fst $ M.findWithDefault (S.empty, S.empty) e vs
                    newnewq = if newin /= oldin then Q.enqueueAll (predecessors e g) newq else newq
                    newvs = M.insert e (newin, out) vs
                in dataFlowRec newnewq newvs
    in dataFlowRec (Q.enqueueAll (S.toList nodes) Q.empty) variables