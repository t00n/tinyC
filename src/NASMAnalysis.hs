module NASMAnalysis where

import TACAnalysis
import TACGenerator
import NASMGenerator
import qualified Data.Map as M
import qualified Data.Set as S

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