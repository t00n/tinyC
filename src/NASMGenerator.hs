{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}

module NASMGenerator (nasmGenerate, nasmGenerateData, nasmGenerateText, nasmShow, NASMProgram(..), NASMData(..), DataSize(..), NASMInstruction(..), RegisterName(..), RegisterSize(..), Register(..), Address(..), AddressSize(..)) where

import qualified Data.Set as S
import qualified Data.Map as M
import Control.Monad.State
import Data.Char (ord)
import Debug.Trace (traceShow)
import Data.String.Builder
import Text.Printf
import Data.List
import Data.Maybe (fromJust)
import Data.Foldable (foldrM)
import Numeric (showHex)
import Data.Char (toLower)


import TACGenerator
import SymbolTable
import TACAnalysis
import Graph
import Parser (Type(..))

nasmGenerate :: TACProgram -> SymbolTable -> NASMProgram
nasmGenerate p = NASMProgram <$> nasmGenerateData p <*> nasmGenerateText p

nasmGenerateData :: TACProgram -> SymbolTable -> [NASMData]
nasmGenerateData p = evalState (evalStateT (nasmGenerateDataReal (fst p)) M.empty)

nasmGenerateText :: TACProgram -> SymbolTable -> [NASMInstruction]
nasmGenerateText p = evalState (evalStateT (nasmGenerateTextReal (snd p)) M.empty)

nasmGenerateDataReal :: [TACInstruction] -> SRSS [NASMData]
nasmGenerateDataReal = return . (map decl)
    where 
          decl (TACCopy s (TACInt i)) = NASMData s DD [i]

          decl (TACCopy s (TACChar c)) = NASMData s DB [ord c]

          decl (TACArrayDecl s xs) = NASMData s DD (map (\(TACInt i) -> i) xs)


nasmGenerateTextReal :: [TACFunction] -> SRSS [NASMInstruction]
nasmGenerateTextReal functions = mapM nasmGenerateInstructions functions >>= (return . concat)

class Show a => NASMGenerator a where
    nasmGenerateInstructions :: a -> SRSS [NASMInstruction]

nasmGeneratePreFunction :: Label -> Offset -> SRSS [NASMInstruction]
nasmGeneratePreFunction name offset = do
    info <- lift $ gets $ unsafeGetSymbolInfo name
    let allocateLocal = if offset == 0 then [] else [SUB4 (Register SP DWORD) (-offset)]
    if name == "tiny" 
        then return $ LABEL name:allocateLocal
        else return $ [LABEL name, PUSH1 BP, MOV1 DWORD BP SP] ++ allocateLocal ++ [PUSH1 B, PUSH1 SI, PUSH1 DI]

nasmGeneratePostFunction :: Label -> SRSS [NASMInstruction]
nasmGeneratePostFunction "tiny" = return [CALL "_exit"]
nasmGeneratePostFunction name = do
    return [POP1 DI, POP1 SI, POP1 B, MOV1 DWORD SP BP, POP1 BP, RET]

labelToName :: TACInstruction -> String
labelToName (TACLabel x) = x

foldLocal :: Spilled -> M.Map String RegisterName -> Variable -> (M.Map String (RegisterName, VariableLocation), Offset) -> SRSS (M.Map String (RegisterName, VariableLocation), Offset)
foldLocal spilled rMapping var (mapping, offset) = do
    info <- lift (gets (getSymbolInfo var))
    let newoffset = case info of
            Nothing -> offset - 4
            (Just x) -> offset - (if (infoType x) == IntType then 4 else 1)
    if var `elem` spilled
        then return (M.insert var (rMapping M.! var, InStack newoffset) mapping, newoffset)
    else return (M.insert var (rMapping M.! var, InRegister (rMapping M.! var)) mapping, offset)

foldParam :: M.Map String RegisterName -> Variable -> (M.Map String (RegisterName, VariableLocation), Offset) -> SRSS (M.Map String (RegisterName, VariableLocation), Offset)
foldParam rMapping var (mapping, offset) = do
    t <- lift (gets (infoType . (unsafeGetSymbolInfo var)))
    let newoffset = offset + (if t == IntType then 4 else 1)
    return (M.insert var (rMapping M.! var, InStack offset) mapping, newoffset)

instance NASMGenerator TACFunction where
    nasmGenerateInstructions xs = do
        st <- lift get
        case nextDF st of
            Nothing -> lift $ put st
            (Just x) -> lift $ put x
        let funcName = (labelToName . head) xs
        let (varIntMap, spilled, is) = mapVariablesToRegisters xs (length registers)
        let varRegMap = M.map (\v -> registers !! v) varIntMap `M.union` M.fromList (map (\x -> (x, last registers)) spilled)
        let variables = M.keys varRegMap
        st <- lift get
        params <- lift (gets (M.keys . infoParams . (unsafeGetSymbolInfo funcName)))
        let globals = filter (flip nameInParent st) variables
        let locals = (variables \\ globals) \\ params
        (localMapping, offset) <- foldrM (foldLocal spilled varRegMap) (M.empty, 0) locals
        paramMapping <- foldrM (foldParam varRegMap) (M.empty, 8) (reverse params)
        let globalMapping = M.fromList $ map (\x -> (x, (varRegMap M.! x, InMemory x))) globals
        let totalMapping = M.unions $ (fst paramMapping:localMapping:[globalMapping])
        put totalMapping
        pre <- nasmGeneratePreFunction funcName offset
        nasmIS <- (mapM nasmGenerateInstructions ((tail . init) is)) >>= return . concat
        post <- nasmGeneratePostFunction funcName
        return $ pre ++ nasmIS ++ post


instance NASMGenerator TACInstruction where
    nasmGenerateInstructions (TACLabel l) = return [LABEL l]
    nasmGenerateInstructions _ = return []

type SRSS = StateT RegisterState (State SymbolTable)

type RegisterState = M.Map String (RegisterName, VariableLocation)

data VariableLocation = InRegister RegisterName
                      | InMemory Label
                      | InStack Offset
    deriving (Eq, Show)

data NASMProgram = NASMProgram [NASMData] [NASMInstruction]
    deriving (Eq, Show)

data NASMData = NASMData Label DataSize [Int]
    deriving (Eq, Show)

data DataSize = DB | DW | DD
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
                     | SHL3 Register -- Shift by the value in CL
                     | SHL4 Address -- Shift by the value in CL
                     | SHR1 Register Constant -- Constant must be 8-bit value
                     | SHR2 Address Constant -- Constant must be 8-bit value
                     | SHR3 Register -- Shift by the value in CL
                     | SHR4 Address -- Shift by the value in CL
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
registers = [A, C, D, SI, DI, B]

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

instShow :: String -> [String] -> String
instShow inst ops = inst ++ " " ++ intercalate ", " ops

instance NASMShow NASMInstruction where
    nasmShow (LABEL l) = printf "%s:" l
    nasmShow (MOV1 size r1 r2) = instShow "mov" [nasmShow (Register r1 size), nasmShow (Register r2 size)]
    nasmShow (MOV2 reg addr) = instShow "mov" [nasmShow reg, nasmShow addr]
    nasmShow (MOV3 addr reg) = instShow "mov" [nasmShow addr, nasmShow reg]
    nasmShow (MOV4 reg cst) = instShow "mov" [nasmShow reg, nasmShow cst]
    nasmShow (MOV5 size addr cst) = "mov " ++ nasmShow size ++ " " ++ nasmShow addr ++ "," ++ nasmShow cst
    nasmShow (PUSH1 rname ) = instShow "push" [nasmShow (Register rname DWORD)]
    nasmShow (PUSH2 addr) = instShow "push" [nasmShow addr]
    nasmShow (PUSH3 cst) = instShow "push" [nasmShow cst]
    nasmShow (POP1 rname ) = instShow "pop" [nasmShow (Register rname DWORD)]
    nasmShow (POP2 addr) = instShow "pop" [nasmShow addr]
    nasmShow (LEA rname addr ) = instShow "lea" [nasmShow (Register rname DWORD), nasmShow addr]
    nasmShow (ADD1 size r1 r2) = instShow "add" [nasmShow (Register r1 size), nasmShow (Register r2 size)]
    nasmShow (ADD2 reg addr) = instShow "add" [nasmShow reg, nasmShow addr]
    nasmShow (ADD3 addr reg) = instShow "add" [nasmShow addr, nasmShow reg]
    nasmShow (ADD4 reg cst) = instShow "add" [nasmShow reg, nasmShow cst]
    nasmShow (ADD5 addr cst) = instShow "add" [nasmShow addr, nasmShow cst]
    nasmShow (SUB1 size r1 r2) = instShow "sub" [nasmShow (Register r1 size), nasmShow (Register r2 size)]
    nasmShow (SUB2 reg addr) = instShow "sub" [nasmShow reg, nasmShow addr]
    nasmShow (SUB3 addr reg) = instShow "sub" [nasmShow addr, nasmShow reg]
    nasmShow (SUB4 reg cst) = instShow "sub" [nasmShow reg, nasmShow cst]
    nasmShow (SUB5 addr cst) = instShow "sub" [nasmShow addr, nasmShow cst]
    nasmShow (INC1 reg) = instShow "inc" [nasmShow reg]
    nasmShow (INC2 addr) = instShow "inc" [nasmShow addr]
    nasmShow (DEC1 reg) = instShow "dec" [nasmShow reg]
    nasmShow (DEC2 addr) = instShow "dec" [nasmShow addr]
    nasmShow (IMUL1 r1 r2) = instShow "imul" [nasmShow (Register r1 DWORD), nasmShow (Register r2 DWORD)]
    nasmShow (IMUL2 rname addr ) = instShow "imul" [nasmShow (Register rname DWORD), nasmShow addr]
    nasmShow (IMUL3 r1 r2 cst ) = instShow "imul" [nasmShow (Register r1 DWORD), nasmShow (Register r2 DWORD), nasmShow cst]
    nasmShow (IMUL4 rname addr cst ) = instShow "imul" [nasmShow (Register rname DWORD), nasmShow addr, nasmShow cst]
    nasmShow (IDIV1 rname ) = instShow "idiv" [nasmShow (Register rname DWORD)]
    nasmShow (IDIV2 addr) = instShow "idiv" [nasmShow addr]
    nasmShow (AND1 size r1 r2) = instShow "and" [nasmShow (Register r1 size), nasmShow (Register r2 size)]
    nasmShow (AND2 reg addr) = instShow "and" [nasmShow reg, nasmShow addr]
    nasmShow (AND3 addr reg) = instShow "and" [nasmShow addr, nasmShow reg]
    nasmShow (AND4 reg cst) = instShow "and" [nasmShow reg, nasmShow cst]
    nasmShow (AND5 addr cst) = instShow "and" [nasmShow addr, nasmShow cst]
    nasmShow (OR1 size r1 r2) = instShow "or" [nasmShow (Register r1 size), nasmShow (Register r2 size)]
    nasmShow (OR2 reg addr) = instShow "or" [nasmShow reg, nasmShow addr]
    nasmShow (OR3 addr reg) = instShow "or" [nasmShow addr, nasmShow reg]
    nasmShow (OR4 reg cst) = instShow "or" [nasmShow reg, nasmShow cst]
    nasmShow (OR5 addr cst) = instShow "or" [nasmShow addr, nasmShow cst]
    nasmShow (XOR1 size r1 r2) = instShow "xor" [nasmShow (Register r1 size), nasmShow (Register r2 size)]
    nasmShow (XOR2 reg addr) = instShow "xor" [nasmShow reg, nasmShow addr]
    nasmShow (XOR3 addr reg) = instShow "xor" [nasmShow addr, nasmShow reg]
    nasmShow (XOR4 reg cst) = instShow "xor" [nasmShow reg, nasmShow cst]
    nasmShow (XOR5 addr cst) = instShow "xor" [nasmShow addr, nasmShow cst]
    nasmShow (NOT1 reg) = instShow "not" [nasmShow reg]
    nasmShow (NOT2 addr) = instShow "not" [nasmShow addr]
    nasmShow (SHL1 reg cst) = instShow "shl" [nasmShow reg, nasmShow cst]
    nasmShow (SHL2 addr cst) = instShow "shl" [nasmShow addr, nasmShow cst]
    nasmShow (SHL3 reg) = instShow "shl" [nasmShow reg, "cl"]
    nasmShow (SHL4 addr) = instShow "shl" [nasmShow addr, "cl"]
    nasmShow (SHR1 reg cst) = instShow "shr" [nasmShow reg, nasmShow cst]
    nasmShow (SHR2 addr cst) = instShow "shr" [nasmShow addr, nasmShow cst]
    nasmShow (SHR3 reg) = instShow "shr" [nasmShow reg, "cl"]
    nasmShow (SHR4 addr) = instShow "shr" [nasmShow addr, "cl"]
    nasmShow (JMP label) = instShow "jmp" [nasmShow label]
    nasmShow (JE label) = instShow "je" [nasmShow label]
    nasmShow (JNE label) = instShow "jne" [nasmShow label]
    nasmShow (JZ label) = instShow "jz" [nasmShow label]
    nasmShow (JG label) = instShow "jg" [nasmShow label]
    nasmShow (JGE label) = instShow "jge" [nasmShow label]
    nasmShow (JL label) = instShow "jl" [nasmShow label]
    nasmShow (JLE label) = instShow "jle" [nasmShow label]
    nasmShow (CMP1 r1 r2) = instShow "cmp" [nasmShow r1, nasmShow r2]
    nasmShow (CMP2 reg addr) = instShow "cmp" [nasmShow reg, nasmShow addr]
    nasmShow (CMP3 addr reg) = instShow "cmp" [nasmShow addr, nasmShow reg]
    nasmShow (CMP4 reg cst) = instShow "cmp" [nasmShow reg, nasmShow cst]
    nasmShow (CALL l) = printf "call %s" l
    nasmShow (RET) = "ret"
    nasmShow (INTERRUPT cst) = instShow "int" [showHex cst ""]

instance NASMShow AddressSize where
    nasmShow BYTEADDRESS = "dword"
    nasmShow WORDADDRESS = "word"
    nasmShow DWORDADDRESS = "byte"

instance NASMShow Address where
    nasmShow (Address reg mult offset) = "[" ++ nasmShow reg ++ "+" ++ nasmShow mult ++ "*" ++ nasmShow offset ++ "]"
    nasmShow (AddressBase r1 r2 mult offset) = "[" ++ nasmShow r1 ++ "+" ++ nasmShow r2 ++ "*" ++ nasmShow mult ++ "+" ++ nasmShow offset ++ "]"
    nasmShow (AddressLabel label offset) = "[" ++ nasmShow label ++ "+" ++ nasmShow offset ++ "]"
instance NASMShow Multiplier where
    nasmShow = show

instance NASMShow Char where
    nasmShow = show

instance NASMShow DataSize where
    nasmShow = (map toLower) . show

instance NASMShow RegisterName where
    nasmShow = (map toLower) . show

instance NASMShow Register where
    nasmShow (Register name size) = 
        let (prev, next) = case size of
                                DWORD -> ("e", "x")
                                WORD -> ("", "x")
                                MSB -> ("", "h")
                                LSB -> ("", "l")
        in  if name `elem` [SI, DI, BP, SP]
                then prev ++ nasmShow name
            else prev ++ nasmShow name ++ next
