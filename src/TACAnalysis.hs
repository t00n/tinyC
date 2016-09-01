module TACAnalysis where

import qualified Data.Set as S
import qualified Data.Map as M
import Data.List (intercalate)
import Debug.Trace (traceShow, trace)

import qualified Queue as Q
import qualified Graph as G
import TACGenerator
import TACProgram

type InstructionNo = Int
type Variables = [Variable]
type LabelInstructionMapping = M.Map Label InstructionNo
type ControlFlowGraph = G.Graph TACInstruction
type DataFlowIn = S.Set Variable
type DataFlowOut = S.Set Variable
type DataFlowGraph = M.Map InstructionNo (DataFlowIn, DataFlowOut)
type RegisterInterferenceGraph = G.Graph Variable
type ExpressionFlowIn = M.Map TACExpression (Variable, S.Set Variable)
type ExpressionFlowOut = M.Map TACExpression (Variable, S.Set Variable)
type ExpressionFlowGraph = M.Map InstructionNo (ExpressionFlowIn, ExpressionFlowOut)

-- retrieve data variables
retrieveVariable :: TACInstruction -> Variable
retrieveVariable (TACCopy a _) = a
retrieveVariable (TACArrayDecl a _) = a

retrieveVariables :: [TACInstruction] -> Variables
retrieveVariables = map retrieveVariable

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
        (TACArrayDecl var ex) -> (expressionsToSet ex, S.fromList [var])

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

dfgShow :: (Show a, Show b) => ControlFlowGraph -> M.Map InstructionNo (a, b) -> String
dfgShow cfg = (intercalate "\n") . M.elems . (M.mapWithKey f)
    where f k (din, dout) = tacPrint (G.unsafeLookup k cfg) ++ " => " ++ show (din, dout)

registerInterferenceGraph :: DataFlowGraph -> RegisterInterferenceGraph
registerInterferenceGraph vs = graph
    where allsets = ((concatMap (\(s1, s2) -> [s1, s2])) . M.elems) vs
          n = S.foldl (flip G.insertNode) G.empty (S.unions allsets)
          graph = foldr f n allsets
          f x g = foldr (uncurry G.insertEdge) g [(G.find a graph, G.find b graph) | a <- S.toList x, b <- S.toList x, a /= b]

genExpressions :: TACInstruction -> (Maybe TACExpression, (Variable, S.Set Variable))
genExpressions inst = 
    let expressionsToSet = (S.fromList . expressionsNames)
    in
    case inst of 
        (TACBinary v e1 op e2) -> (Just $ TACExpr e1 op e2, (v, expressionsToSet [e1, e2]))
        (TACUnary s op e) -> (Just $ TACUnOp op e, (s, expressionsToSet [e]))
        (TACCopy s e) -> (Just e, (s, expressionsToSet [e]))
        (TACArrayAccess var array ex) -> (Just $ TACArray array ex, (var, S.fromList [array] `S.union` expressionsToSet [ex]))
        _ -> (Nothing, ([], S.empty))

killExpressions :: TACInstruction -> ExpressionFlowIn -> ExpressionFlowIn
killExpressions inst mapping = 
    let removeVar v = M.filter (\(def, used) ->  v == def || v `elem` used) mapping in
    case inst of
        (TACBinary v _ _ _) -> removeVar v
        (TACUnary v _ _) -> removeVar v
        (TACCopy v _) -> removeVar v
        (TACArrayAccess v _ _) -> removeVar v
        _ -> mapping

expressionFlowGraph :: ControlFlowGraph -> ExpressionFlowGraph
expressionFlowGraph cfg = foldr backward (G.fold forward M.empty cfg) (G.keys cfg)
    where forward inst efg = update_succs (M.insert inst (flowin, newflowout) efg)
            where (flowin, flowout) = M.findWithDefault (M.empty, M.empty) inst efg
                  instValue = G.unsafeLookup inst cfg
                  (expr, (def, used)) = genExpressions instValue
                  kill = killExpressions instValue flowin
                  newexpr = case expr of 
                                 Nothing -> M.empty
                                 (Just e) -> M.singleton e (def, used)
                  newflowout = (flowin `M.difference` kill) `M.union` newexpr
                  update_succs m = foldr (\k acc -> if k `M.member` acc then M.adjust (\(_, y) -> (newflowout, y)) k acc else M.insert k (newflowout, M.empty) acc) m (G.successors inst cfg)
          backward inst efg = M.adjust (\(flowin, flowout) -> (newflowin, flowout)) inst efg
             where newflowin = mapIntersections . S.toList $ S.map (\k -> let (_, out) = efg M.! k in out) (G.predecessors inst cfg)

mapIntersections :: Ord k => [M.Map k a] -> M.Map k a
mapIntersections [] = M.empty
mapIntersections maps = foldr1 M.intersection maps
