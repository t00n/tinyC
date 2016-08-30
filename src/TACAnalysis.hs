module TACAnalysis where

import qualified Data.Set as S
import qualified Data.Map as M
import qualified Queue as Q
import qualified Graph as G
import Data.List (minimumBy, nub, intercalate, group, sort)
import Data.Ord
import Debug.Trace (traceShow, trace)
import Control.Arrow ((&&&))

import TACGenerator

type InstructionNo = Int
type Label = String
type Variable = String
type Variables = [Variable]
type Spilled = [Variable]
type LabelInstructionMapping = M.Map Label InstructionNo
type ControlFlowGraph = G.Graph TACInstruction
type DataFlowIn = S.Set Variable
type DataFlowOut = S.Set Variable
type DataFlowGraph = M.Map InstructionNo (DataFlowIn, DataFlowOut)
type RegisterInterferenceGraph = G.Graph Variable
type K = Int
type RegisterNo = Int
type RegisterMapping = M.Map Variable RegisterNo
type RegisterConstraintsInt = M.Map Variable (S.Set RegisterNo)

-- map labels to instruction
constructLabelKey :: TACFunction -> LabelInstructionMapping
constructLabelKey is = 
    let isLabel (_, TACLabel _) = True
        isLabel (_, _) = False
    in M.fromList $ map (\(i, TACLabel x) -> (x, i)) $ filter isLabel $ zip [0..] is

controlFlowGraph2 :: LabelInstructionMapping -> ControlFlowGraph -> ControlFlowGraph
controlFlowGraph2 labels graph = 
    let ns = G.nodes graph
        size = M.size ns
        insertLabel k label g = G.insertEdge k (labels M.! label) g
        f k (TACIf _ s) g = 
            if k + 1 < size then ((G.insertEdge k (k+1)) . (insertLabel k s)) g
            else insertLabel k s g
        f k (TACGoto s) g = insertLabel k s g
        f k (TACReturn _) g = g
        f k _ g = 
            if k + 1 < size then G.insertEdge k (k+1) g
            else g
    in M.foldrWithKey f graph ns

controlFlowGraph :: TACFunction -> ControlFlowGraph
controlFlowGraph is = 
    let graph = foldl (flip G.insertNode) G.empty is
    in controlFlowGraph2 (constructLabelKey is) graph

expressionsNames :: [TACExpression] -> [Variable]
expressionsNames es = 
    let
        expName (TACVar n) = [n]
        expName (TACArray v e) = v:expressionsNames [e]
        expName (TACExpr e1 _ e2) = concatMap expName [e1, e2]
        expName _ = []
    in concatMap expName es

usedAndDefinedVariables :: TACInstruction -> (S.Set String, S.Set String)
usedAndDefinedVariables inst = 
    let expressionsToSet = (S.fromList . expressionsNames)
    in
    case inst of 
        (TACBinary v e1 _ e2) -> (expressionsToSet [e1, e2], S.fromList [v])
        (TACUnary s _ e) -> (expressionsToSet [e], S.fromList [s])
        (TACCopy s e) -> (expressionsToSet [e], S.fromList [s])
        (TACArrayAccess var array ex) -> (expressionsToSet [TACVar array, ex], S.fromList [var])
        (TACArrayModif array index ex) -> (expressionsToSet [index, ex] `S.union` S.fromList [array], S.empty)
        (TACIf e _) -> (expressionsToSet [e], S.empty)
        (TACCall _ es ret) -> case ret of
                                   Nothing -> (expressionsToSet es, S.empty)
                                   (Just x) -> (expressionsToSet es, expressionsToSet [x])
        (TACReturn Nothing) -> (S.empty, S.empty)
        (TACReturn (Just e)) -> (expressionsToSet [e], S.empty)
        (TACWrite _ e) -> (expressionsToSet [e], S.empty)
        (TACRead _ e) -> (S.empty, expressionsToSet [e])
        (TACLoad s) -> (S.empty, S.fromList [s])
        (TACStore s) -> (S.fromList [s], S.empty)
        (TACAddress v e) -> (expressionsToSet [e], S.fromList [v])
        (TACDeRef v e) -> (expressionsToSet [e], S.fromList [v])
        (TACDeRefA v e) -> (expressionsToSet [e] `S.union` S.fromList [v], S.empty)
        (TACGoto _) -> (S.empty, S.empty)
        (TACLabel _) -> (S.empty, S.empty)
        (TACArrayDecl var ex) -> (S.empty, S.empty)

dataFlowGraph :: ControlFlowGraph -> DataFlowGraph
dataFlowGraph cfg = 
    let variables = M.fromSet (\k -> (S.empty, S.empty)) (G.keysSet cfg)
        dataFlowGraphRec q vs
            | null q = vs
            | otherwise = 
                let (Just i, newq) = Q.dequeue q
                    inst = G.unsafeLookup i cfg
                    succs = M.filterWithKey (\k _ -> k `elem` G.successors i cfg) vs
                    out = S.unions $ M.elems $ M.map fst succs
                    (use, def) = usedAndDefinedVariables inst
                    newin = S.union use (S.difference out def)
                    oldin = fst $ M.findWithDefault (S.empty, S.empty) i vs
                    newnewq = if newin /= oldin then Q.enqueueAll (S.toList $ G.predecessors i cfg) newq else newq
                    newvs = M.insert i (newin, out) vs
                in dataFlowGraphRec newnewq newvs
    in dataFlowGraphRec (Q.enqueueAll (G.keys cfg) Q.empty) variables

dfgShow :: ControlFlowGraph -> DataFlowGraph -> String
dfgShow cfg = (intercalate "\n") . M.elems . (M.mapWithKey f)
    where f k (din, dout) = tacPrint (G.unsafeLookup k cfg) ++ " => " ++ show dout

registerInterferenceGraph :: DataFlowGraph -> RegisterInterferenceGraph
registerInterferenceGraph vs = graph
    where allsets = ((concatMap (\(s1, s2) -> [s1, s2])) . M.elems) vs
          n = S.foldl (flip G.insertNode) G.empty (S.unions allsets)
          graph = foldr f n allsets
          f x g = foldr (uncurry G.insertEdge) g [(G.find a graph, G.find b graph) | a <- S.toList x, b <- S.toList x, a /= b]

simplifyRIG2 :: Variables -> Spilled -> RegisterInterferenceGraph -> K -> (Variables, Spilled)
simplifyRIG2 xs spills g k
    | null (G.nodes g) = (xs, spills)
    | otherwise =
        let node = fst $ minimumBy (comparing snd) (S.map (\x -> (x, length (G.neighbours x g))) (G.keysSet g))
        in  if length (G.neighbours node g) < k
                then simplifyRIG2 (G.unsafeLookup node g:xs) spills (G.delete node g) k
            else simplifyRIG2 (G.unsafeLookup node g:xs) (G.unsafeLookup node g:spills) (G.delete node g) k

simplifyRIG :: RegisterInterferenceGraph -> K -> (Variables, Spilled)
simplifyRIG = simplifyRIG2 [] []

frequency :: (Eq a, Ord a) => [a] -> [(a, Int)]
frequency = map (head &&& length) . group . sort

spillMoreVariables :: TACFunction -> Spilled -> DataFlowGraph -> K -> Spilled
spillMoreVariables is spilled dfg k
    | and (M.map (\(vin, _) -> S.size vin <= k) dfg) = spilled
    | otherwise = spillMoreVariables newfunction newspilled newdfg k
                where usedPlusDefOf = (\(a, b) -> S.toList $ a `S.union` b) . usedAndDefinedVariables . ((!!) is)
                      varFrequency = (frequency . concat . M.elems) $ M.mapWithKey (\k a -> (filter (`notElem` usedPlusDefOf k) . filter (`notElem` spilled) . S.toList . fst) a) $ M.filter (((<) k) . S.size . fst) dfg
                      varToSpill = fst $ minimumBy (comparing snd) varFrequency
                      newspilled = varToSpill:spilled
                      newfunction = fixInstructions is [varToSpill]
                      newdfg = (dataFlowGraph . controlFlowGraph) newfunction

findRegister :: Variable -> Variables -> K -> RegisterMapping -> RegisterConstraintsInt -> Int
findRegister node neigh k mapping negConstraints = 
    let registersUsed = M.elems $ M.filterWithKey (\k v -> k `elem` neigh) mapping
        registersAvailable = [x | x <- [0..(k-1)], not (x `elem` registersUsed), node `M.notMember` negConstraints || x `S.notMember` (negConstraints M.! node)]
    in if length registersAvailable > 0 then head registersAvailable else error $ "findRegister : head : empty list " ++ show (node, neigh, k, mapping, negConstraints)

findRegisters2 :: Variables -> RegisterInterferenceGraph -> K -> RegisterMapping -> RegisterConstraintsInt -> RegisterMapping
findRegisters2 [] _ _ mapping _ = mapping
findRegisters2 (n:ns) g k mapping negConstraints = 
    let register = findRegister n (map (flip G.unsafeLookup g) $ S.toList $ G.neighbours (G.find n g) g) k mapping negConstraints
    in findRegisters2 ns g k (M.insert n register mapping) negConstraints

findRegisters :: Variables -> RegisterInterferenceGraph -> K -> RegisterConstraintsInt -> RegisterMapping
findRegisters nodes g k negConstraints = findRegisters2 nodes g k M.empty negConstraints

fixInstructions :: TACFunction -> Spilled -> TACFunction
fixInstructions is spilled = concatMap f is
    where f inst = load ++ [inst] ++ store
            where (used, def) = usedAndDefinedVariables inst
                  load = foldr (\x acc -> if S.member x used then (TACLoad x:acc) else acc) [] spilled
                  store = foldr (\x acc -> if S.member x def then (TACStore x:acc) else acc) [] spilled

findAddressedVariables :: TACFunction -> Spilled
findAddressedVariables = (nub . (concatMap f))
    where f (TACAddress v e) = expressionsNames [e]
          f _ = []

mapVariablesToRegisters :: TACFunction -> K -> RegisterConstraintsInt -> (RegisterMapping, Spilled, TACFunction)
mapVariablesToRegisters function k negConstraints = let 
        addressedVariables = findAddressedVariables function
        rig = (registerInterferenceGraph . dataFlowGraph . controlFlowGraph) function
        (variables, spilled) = simplifyRIG rig k
        allspilled = nub $ spilled ++ addressedVariables
        intermediateIS = fixInstructions function allspilled
        dfg = (dataFlowGraph . controlFlowGraph) intermediateIS
        newspilled = spillMoreVariables intermediateIS allspilled dfg k
        newfunction = fixInstructions function newspilled
        newrig = (registerInterferenceGraph . dataFlowGraph . controlFlowGraph) newfunction
    in  if allspilled == [] then (findRegisters variables rig k negConstraints, [], function)
        else (findRegisters variables newrig k negConstraints, newspilled, newfunction)