module TACAnalysis where

import qualified Data.Set as S
import qualified Data.Map as M
import qualified Queue as Q
import Data.List (maximumBy)
import Data.Ord
import Debug.Trace (traceShow, trace)

import TACGenerator
import Graph

type Label = String
type Variable = String
type Variables = [Variable]
type Spilled = [Variable]
type LabelInstructionMapping = M.Map Label TACInstruction
type ControlFlowGraph = Graph TACInstruction
type DataFlowIn = S.Set Variable
type DataFlowOut = S.Set Variable
type DataFlowGraph = M.Map TACInstruction (DataFlowIn, DataFlowOut)
type RegisterInterferenceGraph = Graph Variable
type K = Int
type RegisterMapping = M.Map Variable Int

-- map labels to instruction
constructLabelKey :: TACFunction -> LabelInstructionMapping
constructLabelKey is = 
    let isLabel (TACLabel _) = True
        isLabel _ = False
    in M.fromList $ map (\(TACLabel x) -> (x, TACLabel x)) $ filter isLabel is

controlFlowGraph2 :: TACFunction -> LabelInstructionMapping -> ControlFlowGraph -> ControlFlowGraph
controlFlowGraph2 instructions labels g = 
    let insertLabel curr label g = insertEdge curr (labels M.! label) g
        updateGraph curr next = 
            let graphwithnode = insertNode curr g in
            case curr of
                TACIf _ s -> case next of 
                    (Just x) -> ((insertLabel curr s) . (insertEdge curr x)) graphwithnode
                    Nothing -> insertLabel curr s graphwithnode
                TACGoto s -> insertLabel curr s graphwithnode
                TACReturn _ -> graphwithnode
                _ -> case next of 
                        (Just x) -> insertEdge curr x graphwithnode
                        Nothing -> graphwithnode
    in
    case instructions of
        (x:y:xs) -> controlFlowGraph2 (y:xs) labels (updateGraph x (Just y))
        (x:xs) -> controlFlowGraph2 xs labels (updateGraph x Nothing)
        [] -> g

controlFlowGraph :: TACFunction -> ControlFlowGraph
controlFlowGraph is = controlFlowGraph2 is (constructLabelKey is) emptyGraph


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

dataFlowGraph :: ControlFlowGraph -> DataFlowGraph
dataFlowGraph g@(Graph nodes edges) = 
    let variables = M.fromSet (\k -> (S.empty :: DataFlowIn, S.empty :: DataFlowOut)) nodes
        dataFlowGraphRec q vs
            | null q = vs
            | otherwise = 
                let (Just inst, newq) = Q.dequeue q
                    succs = M.filterWithKey (\k _ -> k `elem` successors inst g) vs
                    out = S.unions $ M.elems $ M.map fst succs
                    (use, def) = usedAndDefinedVariables inst
                    newin = S.union use (S.difference out def)
                    oldin = fst $ M.findWithDefault (S.empty, S.empty) inst vs
                    newnewq = if newin /= oldin then Q.enqueueAll (S.toList $ predecessors inst g) newq else newq
                    newvs = M.insert inst (newin, out) vs
                in dataFlowGraphRec newnewq newvs
    in dataFlowGraphRec (Q.enqueueAll (S.toList nodes) Q.empty) variables

registerInterferenceGraph :: DataFlowGraph -> RegisterInterferenceGraph
registerInterferenceGraph vs = Graph nodes edges
    where allsets = ((concatMap (\(s1, s2) -> [s1, s2])) . M.elems) vs
          nodes = S.unions allsets
          edges = foldr f S.empty allsets
          f x g = foldr S.insert g [(a, b) | a <- S.toList x, b <- S.toList x, a /= b]

simplifyRIG2 :: Variables -> Spilled -> RegisterInterferenceGraph -> K -> (Variables, Spilled)
simplifyRIG2 xs spills g@(Graph nodes _) k = 
    let isSimplifiable x Nothing = if length (neighbours x g) < k then (Just x) else Nothing
        isSimplifiable _ (Just x) = (Just x)
        findSimplify = S.foldr isSimplifiable Nothing nodes
        findSpill = fst $ maximumBy (comparing snd) (S.map (\x -> (x, length (neighbours x g))) nodes)
    in  if null nodes then (xs, spills)
        else case findSimplify of
            Nothing -> simplifyRIG2 xs (findSpill:spills) (deleteNode findSpill g) k
            (Just x) -> simplifyRIG2 (x:xs) spills (deleteNode x g) k

simplifyRIG :: RegisterInterferenceGraph -> K -> (Variables, Spilled)
simplifyRIG = simplifyRIG2 [] []

findRegister :: Variable -> Variables -> K -> RegisterMapping -> Int
findRegister node neigh k mapping = 
    let registersUsed = M.elems $ M.filterWithKey (\k v -> k `elem` neigh) mapping
    in head $ [x | x <- [0..(k-1)], not (x `elem` registersUsed)]

findRegisters2 :: Variables -> RegisterInterferenceGraph -> K -> RegisterMapping -> RegisterMapping
findRegisters2 [] _ _ mapping = mapping
findRegisters2 (n:ns) g k mapping = 
    let register = findRegister n (S.toList $ neighbours n g) k mapping
    in findRegisters2 ns g k (M.insert n register mapping)

findRegisters :: Variables -> RegisterInterferenceGraph -> K -> RegisterMapping
findRegisters nodes g k = findRegisters2 nodes g k M.empty

fixInstructions :: TACFunction -> Spilled -> TACFunction
fixInstructions is spilled = concatMap f is
    where f inst = load ++ [inst] ++ store
            where (used, def) = usedAndDefinedVariables inst
                  load = foldr (\x acc -> if S.member x used then (TACLoad x:acc) else acc) [] spilled
                  store = foldr (\x acc -> if S.member x def then (TACStore x:acc) else acc) [] spilled

mapVariablesToRegisters :: TACFunction -> K -> (RegisterMapping, Spilled, TACFunction)
mapVariablesToRegisters is k = 
    let cfg = controlFlowGraph is
        df = dataFlowGraph cfg
        rig = registerInterferenceGraph df
        (nodes, spilled) = simplifyRIG rig k
        (newnodes, newspilled) = simplifyRIG rig (k-1)
        newis = fixInstructions is newspilled
        newrig = (registerInterferenceGraph . dataFlowGraph . controlFlowGraph) newis
    in
    if spilled == []
        then (findRegisters nodes rig k, [], is)
    else (findRegisters newnodes newrig (k-1), newspilled, newis)