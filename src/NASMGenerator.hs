{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}

module NASMGenerator (nasmGenerate, nasmGenerateData, nasmGenerateText, nasmShow, NASMProgram(..), NASMData(..), NASMInstruction(..), RegisterName(..), RegisterSize(..), Register(..), Address(..), AddressSize(..)) where

import qualified Data.Set as S
import qualified Data.Map as M
import Control.Monad.State
import Data.Char (ord)
import Debug.Trace (traceShow)
import Data.String.Builder
import Text.Printf
import Data.List
import Data.Maybe (fromJust)


import TACGenerator
import SymbolTable
import TACAnalysis
import Graph

nasmGenerate :: TACProgram -> SymbolTable -> NASMProgram
nasmGenerate p = NASMProgram <$> nasmGenerateData p <*> nasmGenerateText p

nasmGenerateData :: TACProgram -> SymbolTable -> [NASMData]
nasmGenerateData p = evalState (evalStateT (nasmGenerateDataReal (fst p)) M.empty)

nasmGenerateText :: TACProgram -> SymbolTable -> [NASMInstruction]
nasmGenerateText p = evalState (evalStateT (nasmGenerateTextReal (snd p)) M.empty)

nasmGenerateDataReal :: [TACInstruction] -> SRSS [NASMData]
nasmGenerateDataReal = return . (map decl)
    where 
          decl (TACCopy s (TACInt i)) = NASMData s DWORDADDRESS [i]

          decl (TACCopy s (TACChar c)) = NASMData s BYTEADDRESS [ord c]

          decl (TACArrayDecl s xs) = NASMData s DWORDADDRESS (map (\(TACInt i) -> i) xs)


nasmGenerateTextReal :: [TACFunction] -> SRSS [NASMInstruction]
nasmGenerateTextReal functions = mapM nasmGenerateInstructions functions >>= (return . concat)

class Show a => NASMGenerator a where
    nasmGenerateInstructions :: a -> SRSS [NASMInstruction]

nasmGeneratePreFunction :: Label -> SRSS [NASMInstruction]
nasmGeneratePreFunction "tiny" = return [LABEL "tiny"]
nasmGeneratePreFunction name = do
    info <- lift $ gets $ unsafeGetSymbolInfo name
    return [LABEL name]

nasmGeneratePostFunction :: Label -> SRSS [NASMInstruction]
nasmGeneratePostFunction "tiny" = return [CALL "_exit"]
nasmGeneratePostFunction name = do
    return [RET]

labelToName :: TACInstruction -> String
labelToName (TACLabel x) = x

mapParameter :: RegisterMapping -> Symbols -> Variable -> (Variable, VariableLocation)
mapParameter varInRegisters funcParams p = 
    if p `elem` M.keys varInRegisters
        then (p, InRegister (registers !! fromJust (M.lookup p varInRegisters)))
    else case elemIndex p (M.keys funcParams) of
        Nothing -> error "no parameter lolol"
        (Just x) -> (p, InStack (8+x*4))

mapLocal :: RegisterMapping -> Spilled -> Variable -> (Variable, VariableLocation)
mapLocal varInRegisters varSpilled p = 
    if p `elem` M.keys varInRegisters
        then (p, InRegister (registers !! fromJust (M.lookup p varInRegisters)))
    else case elemIndex p varSpilled of
        Nothing -> error "no local lolol"
        (Just x) -> (p, InStack (-4-x*4))

instance NASMGenerator TACFunction where
    nasmGenerateInstructions xs = do
        let funcName = (labelToName . head) xs
        funcInfo <- lift $ gets $ unsafeGetSymbolInfo funcName
        let funcParams = infoParams funcInfo
        let rig@(Graph variables _) = (registerInterferenceGraph . dataFlowGraph . controlFlowGraph) xs
        let registerMapping = mapVariablesToRegisters xs (length registers)
        let spilled = S.toList $ S.filter (not . (`elem` M.keys registerMapping)) variables
        let parametersMapping = M.fromList $ map (mapParameter registerMapping funcParams) (M.keys funcParams)
        let localMapping = M.fromList $ map (mapLocal registerMapping spilled) (S.toList variables)
        let variableMapping = M.unions [parametersMapping, localMapping]
        put variableMapping
        pre <- nasmGeneratePreFunction funcName
        nasmIS <- (mapM nasmGenerateInstructions ((tail . init) xs)) >>= return . concat
        post <- nasmGeneratePostFunction funcName
        traceShow variableMapping $ return $ pre ++ nasmIS ++ post


instance NASMGenerator TACInstruction where
    nasmGenerateInstructions (TACLabel l) = return [LABEL l]
    nasmGenerateInstructions _ = return []

type SRSS = StateT RegisterState (State SymbolTable)

type RegisterState = M.Map String VariableLocation

data VariableLocation = InRegister RegisterName
                      | InMemory Label
                      | InStack Offset
    deriving (Eq, Show)

data NASMProgram = NASMProgram [NASMData] [NASMInstruction]
    deriving (Eq, Show)

data NASMData = NASMData Label AddressSize [Int]
    deriving (Eq, Show)

data NASMInstruction = LABEL Label
                     | MOV1 RegisterSize RegisterName RegisterName
                     | MOV2 Register Address
                     | MOV3 Address Register
                     | MOV4 Register Int
                     | MOV5 AddressSize Address Constant
                     | PUSH1 RegisterName -- Only 32-bit register
                     | PUSH2 Address
                     | PUSH3 Constant -- 32-bit constant
                     | POP1 RegisterName -- Only 32-bit register
                     | POP2 Address
                     | LEA RegisterName Address -- Only 32-bit register
                     | ADD1 RegisterSize RegisterName RegisterName
                     | ADD2 Register Address
                     | ADD3 Address Register
                     | ADD4 Register Constant
                     | ADD5 Address Constant
                     | SUB1 RegisterSize RegisterName RegisterName
                     | SUB2 Register Address
                     | SUB3 Address Register
                     | SUB4 Register Constant
                     | SUB5 Address Constant
                     | INC1 Register
                     | INC2 Address
                     | DEC1 Register
                     | DEC2 Address
                     | IMUL1 RegisterName RegisterName -- Only 32-bit register
                     | IMUL2 RegisterName Address -- Only 32-bit register
                     | IMUL3 RegisterName RegisterName Constant -- Only 32-bit register
                     | IMUL4 RegisterName Address Constant -- Only 32-bit register
                     -- divide the contents of EDX:EAX by the contents of the parameter. Place the quotient in EAX and the remainder in EDX.
                     | IDIV1 RegisterName -- Only 32-bit register
                     | IDIV2 Address
                     | AND1 RegisterSize RegisterName RegisterName
                     | AND2 Register Address
                     | AND3 Address Register
                     | AND4 Register Constant
                     | AND5 Address Constant
                     | OR1 RegisterSize RegisterName RegisterName
                     | OR2 Register Address
                     | OR3 Address Register
                     | OR4 Register Constant
                     | OR5 Address Constant
                     | XOR1 RegisterSize RegisterName RegisterName
                     | XOR2 Register Address
                     | XOR3 Address Register
                     | XOR4 Register Constant
                     | XOR5 Address Constant
                     | NOT1 Register
                     | NOT2 Address
                     | SHL1 Register Constant -- Constant must be 8-bit value
                     | SHL2 Address Constant -- Constant must be 8-bit value
                     | SHL3 Register Register -- Second Register must be CL
                     | SHL4 Address Register -- Second Register must be CL
                     | SHR1 Register Constant -- Constant must be 8-bit value
                     | SHR2 Address Constant -- Constant must be 8-bit value
                     | SHR3 Register Register -- Second Register must be CL
                     | SHR4 Address Register -- Second Register must be CL
                     | JMP Label
                     | JE Label
                     | JNE Label
                     | JZ Label
                     | JG Label
                     | JGE Label
                     | JL Label
                     | JLE Label
                     | CMP1 Register Register
                     | CMP2 Register Address
                     | CMP3 Address Register
                     | CMP4 Register Constant
                     | CALL Label
                     | RET
                     | INTERRUPT Constant
    deriving (Eq, Show)

data RegisterSize = LSB | MSB | WORD | DWORD
    deriving (Eq, Show)

data RegisterName = A | B | C | D | SI | DI | SP | BP
    deriving (Eq, Show, Ord)

registers :: [RegisterName]
registers = [A, B, C, D, SI, DI]

data Register = Register RegisterName RegisterSize
    deriving (Eq, Show)

data AddressSize = BYTEADDRESS | WORDADDRESS | DWORDADDRESS
    deriving (Eq, Show)

data Address = Address Register Multiplier Offset
             | AddressBase Register Register Multiplier Offset
             | AddressLabel Label Offset
    deriving (Eq, Show)

type Constant = Int

type Multiplier = Int

type Offset = Int

class NASMShow a where
    nasmShow :: a -> String

instance NASMShow a => NASMShow [a] where
    nasmShow = concatMap (\x -> nasmShow x ++ "\n")

instance NASMShow NASMProgram where
    nasmShow (NASMProgram ds is) = build $ do
        "global tiny"
        "section .data"
        "\talign 2"
        literal $ nasmShow ds
        "section .bss"
        "section .text"
        literal $ nasmShow is
        "_exit:"
        "mov eax, 1"
        "push dword 0"
        "push eax"
        "int 0x80"
 
instance NASMShow NASMData where
    nasmShow (NASMData l size vs) = printf "\t%s: %s\t" l (nasmShow size) ++ intercalate "," [printf "%d" v | v <- vs]

instance NASMShow NASMInstruction where
    nasmShow (LABEL l) = printf "%s:" l
    nasmShow (RET) = "ret"
    nasmShow (CALL l) = printf "call %s" l

instance NASMShow AddressSize where
    nasmShow BYTEADDRESS = "db"
    nasmShow WORDADDRESS = "dw"
    nasmShow DWORDADDRESS = "dd"