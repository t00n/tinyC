module TACAnalysis where

import qualified Data.Set as S
import qualified Data.Map as M
import qualified Queue as Q
import Data.List (maximumBy)
import Data.Ord
import Debug.Trace (traceShow, trace)

import TACGenerator
import Graph

constructLabelKey :: [TACInstruction] -> M.Map String Int
constructLabelKey is = 
    let isLabel (TACLabel _, _) = True
        isLabel (_, _) = False
    in M.fromList $ map (\(TACLabel x, i) -> (x, i)) $ filter isLabel (zip is [0..])

type ControlFlowGraph = Graph TACInstruction

controlFlowGraph2 :: [TACInstruction] -> Int -> M.Map String Int -> ControlFlowGraph -> ControlFlowGraph
controlFlowGraph2 [] _ _ g = g
controlFlowGraph2 (x:xs) i labels g =
    let graphplusnode = insertNode i x g
        graphplusedges = case x of
                        TACIf _ s -> ((insertEdge i (labels M.! s)) . (insertEdge i (i+1))) graphplusnode
                        TACGoto s -> insertEdge i (labels M.! s) graphplusnode
                        TACReturn _ -> graphplusnode
                        _ -> insertEdge i (i+1) graphplusnode
    in controlFlowGraph2 xs (i+1) labels graphplusedges

controlFlowGraph :: [TACInstruction] -> ControlFlowGraph
controlFlowGraph is = controlFlowGraph2 is 0 (constructLabelKey is) emptyGraph


usedAndDefinedVariables :: TACInstruction -> (S.Set String, S.Set String)
usedAndDefinedVariables inst = 
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
        (TACCall _ es) -> (expressionsToSet (init es), expressionsToSet [last es])
        (TACReturn Nothing) -> (S.empty, S.empty)
        (TACReturn (Just e)) -> (expressionsToSet [e], S.empty)
        (TACWrite e) -> (expressionsToSet [e], S.empty)
        (TACRead e) -> (S.empty, expressionsToSet [e])
        (TACLoad s) -> (S.empty, S.fromList [s])
        (TACStore s) -> (S.fromList [s], S.empty)
        _ -> (S.empty, S.empty)

type DataFlowGraph = M.Map Int (S.Set String, S.Set String)

dataFlowGraph :: ControlFlowGraph -> DataFlowGraph
dataFlowGraph g@(Graph nodes edges values) = 
    let variables = M.fromSet (\k -> (S.empty :: S.Set String, S.empty :: S.Set String)) nodes
        dataFlowGraphRec q vs
            | null q = vs
            | otherwise = 
                let (Just e, newq) = Q.dequeue q
                    succs = M.filterWithKey (\k _ -> k `elem` successors e g) vs
                    out = S.unions $ M.elems $ M.map (\(invar, outvar) -> invar) succs
                    (Just inst) = lookupNode e g
                    (use, def) = usedAndDefinedVariables inst
                    newin = S.union use (S.difference out def)
                    oldin = fst $ M.findWithDefault (S.empty, S.empty) e vs
                    newnewq = if newin /= oldin then Q.enqueueAll (S.toList $ predecessors e g) newq else newq
                    newvs = M.insert e (newin, out) vs
                in dataFlowGraphRec newnewq newvs
    in dataFlowGraphRec (Q.enqueueAll (S.toList nodes) Q.empty) variables

type RegisterInterferenceGraph = Graph String

registerInterferenceGraph :: DataFlowGraph -> RegisterInterferenceGraph
registerInterferenceGraph vs = Graph (S.fromList ids) edges values
    where allsets = ((concatMap (\(s1, s2) -> [s1, s2])) . M.elems) vs
          nodes = (S.toList . S.unions) allsets
          ids = [0..(length nodes)-1]
          valueIntMap = M.fromList (zip nodes ids)
          values = M.fromList (zip ids nodes)
          edges = foldr f S.empty allsets
          f x g = foldr S.insert g [(a1, b1) | a <- S.toList x, b <- S.toList x, a /= b, let (Just a1) = M.lookup a valueIntMap, let (Just b1) = M.lookup b valueIntMap]

type Variables = [Int]
type Spilled = [Int]
type K = Int

simplifyRIG2 :: Variables -> Spilled -> RegisterInterferenceGraph -> K -> (Variables, Spilled)
simplifyRIG2 xs spills g@(Graph nodes _ _) k = 
    let isSimplifiable x Nothing = if length (neighbours x g) < k then (Just x) else Nothing
        isSimplifiable _ (Just x) = (Just x)
        findSimplify = S.foldr isSimplifiable Nothing nodes
        findSpill = fst $ maximumBy (comparing snd) (S.map (\x -> (x, length (neighbours x g))) nodes)
    in  if null nodes then (xs, spills)
        else case findSimplify of
            Nothing -> simplifyRIG2 xs (findSpill:spills) (delete findSpill g) k
            (Just x) -> simplifyRIG2 (x:xs) spills (delete x g) k

simplifyRIG :: RegisterInterferenceGraph -> K -> (Variables, Spilled)
simplifyRIG = simplifyRIG2 [] []

type RegisterMapping = M.Map Node Int

findRegister :: Node -> [Node] -> K -> RegisterMapping -> Int
findRegister node neigh k mapping = 
    let registersUsed = M.elems $ M.filterWithKey (\k v -> k `elem` neigh) mapping
    in head $ [x | x <- [0..(k-1)], not (x `elem` registersUsed)]

findRegisters2 :: [Node] -> RegisterInterferenceGraph -> K -> RegisterMapping -> RegisterMapping
findRegisters2 [] _ _ mapping = mapping
findRegisters2 (n:ns) g k mapping = 
    let register = findRegister n (S.toList $ neighbours n g) k mapping
    in findRegisters2 ns g k (M.insert n register mapping)

findRegisters :: [Node] -> RegisterInterferenceGraph -> K -> RegisterMapping
findRegisters nodes g k = findRegisters2 nodes g k M.empty

fixInstructions :: TACFunction -> [String] -> TACFunction
fixInstructions is spilled = concatMap f is
    where f inst = load ++ [inst] ++ store
            where (used, def) = usedAndDefinedVariables inst
                  load = foldr (\x acc -> if S.member x used then (TACLoad x:acc) else acc) [] spilled
                  store = foldr (\x acc -> if S.member x def then (TACStore x:acc) else acc) [] spilled

mapVariablesToRegisters :: TACFunction -> K -> RegisterMapping
mapVariablesToRegisters is k = 
    let cfg = controlFlowGraph is
        df = dataFlowGraph cfg
        rig = registerInterferenceGraph df
        (nodes, spilled) = simplifyRIG rig k
        (newnodes, newspilled) = simplifyRIG rig (k-1)
        newis = fixInstructions is (map (flip unsafeLookupNode rig) newspilled)
    in
    if spilled == []
        then findRegisters nodes rig k
    else findRegisters newnodes ((registerInterferenceGraph . dataFlowGraph . controlFlowGraph) newis) (k-1)