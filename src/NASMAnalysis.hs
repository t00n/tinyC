module NASMAnalysis where

import TACAnalysis
import TACProgram
import NASMProgram
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Graph as G
import Data.Maybe (fromJust)
import Data.List (minimumBy, nub, group, sort, elemIndex)
import Control.Arrow ((&&&))
import Data.Ord (comparing)

type K = Int
type RegisterNo = Int
type Spilled = [Variable]
type RegisterMapping = M.Map Variable RegisterNo
type RegisterConstraintsInt = M.Map Variable (S.Set RegisterNo)
type RegisterState = M.Map String (RegisterName, VariableLocation)

data VariableLocation = InRegister RegisterName
                      | InMemory Label
                      | InStack Offset
    deriving (Eq, Show)

type RegisterConstraints = M.Map Variable (S.Set RegisterName)

foldNegConstraints :: TACInstruction -> RegisterConstraints -> RegisterConstraints
foldNegConstraints inst constraints = 
    let addConstraint var reg csts = if M.notMember var csts then M.insert var (S.singleton reg) csts
                                     else M.adjust (S.insert reg) var csts
    in
    case inst of
        (TACBinary _ e1 op e2) -> if op == TACDivide then case e2 of
                                                               (TACVar v) -> ((addConstraint v A) . (addConstraint v D)) constraints
                                                               _ -> constraints
                                  else constraints
        _ -> constraints

negativeConstraints :: TACFunction -> RegisterConstraints
negativeConstraints = foldr foldNegConstraints M.empty


constraintsRegisterNameToInt :: RegisterConstraints -> RegisterConstraintsInt
constraintsRegisterNameToInt = M.map f
    where f = S.map (\x -> fromJust (elemIndex x registers))


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