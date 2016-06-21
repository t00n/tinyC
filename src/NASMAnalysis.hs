module NASMAnalysis where

import TACAnalysis
import TACGenerator
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe (fromJust)
import Data.List (elemIndex)


data RegisterSize = LSB | MSB | WORD | DWORD
    deriving (Eq, Show)

data RegisterName = A | B | C | D | SI | DI | SP | BP
    deriving (Eq, Show, Ord)

registers :: [RegisterName]
registers = [A, C, D, SI, DI, B]

data Register = Register RegisterName RegisterSize
    deriving (Eq, Show)

data AddressSize = BYTEADDRESS | WORDADDRESS | DWORDADDRESS
    deriving (Eq, Show)

data Address = AddressRegisterOffset Register Offset Multiplier
             | AddressRegisterRegister Register Offset Register Multiplier
             | AddressLabelOffset Label Offset Multiplier
             | AddressLabelRegister Label Register Multiplier
    deriving (Eq, Show)

type Constant = Int

type Multiplier = Int

type Offset = Int

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