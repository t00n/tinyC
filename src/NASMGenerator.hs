{-# LANGUAGE OverloadedStrings, FlexibleInstances, OverlappingInstances #-}

module NASMGenerator (nasmGenerate, nasmGenerateData, nasmGenerateText, nasmShow, NASMProgram(..), NASMData(..), DataSize(..), NASMInstruction(..), RegisterName(..), RegisterSize(..), Register(..), Address(..), AddressSize(..), registers) where

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


import TACProgram
import SymbolTable
import TACAnalysis
import Graph
import AST
import NASMAnalysis

nasmGenerate :: SymbolTable -> TACProgram -> NASMProgram
nasmGenerate st = NASMProgram <$> nasmGenerateData st <*> nasmGenerateText st

nasmGenerateData :: SymbolTable -> TACProgram -> [NASMData]
nasmGenerateData st p = evalState (evalStateT (evalStateT (nasmGenerateDataReal (tacData p)) M.empty) st) (Flags False)

nasmGenerateText :: SymbolTable -> TACProgram -> [NASMInstruction]
nasmGenerateText st p = evalState (evalStateT (evalStateT (nasmGenerateTextReal (tacCode p)) M.empty) st) (Flags False)

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
    let commonBeg = [LABEL name, PUSH1 BP, MOV1 DWORD BP SP]
    let allocateLocal = if offset == 0 then [] else [SUB4 (Register SP DWORD) (-offset)]
    if name == "tiny" 
        then (lift $ lift $ put (Flags True)) >> return (commonBeg ++ allocateLocal)
    else return $ commonBeg ++ allocateLocal ++ [PUSH1 B, PUSH1 SI, PUSH1 DI]

nasmGeneratePostFunction :: Label -> SRSS ()
nasmGeneratePostFunction "tiny" = lift $ lift $ put (Flags False)
nasmGeneratePostFunction _ = return ()

labelToName :: TACInstruction -> String
labelToName (TACLabel x) = x

foldLocal :: Spilled -> M.Map String RegisterName -> Variable -> (M.Map String (RegisterName, VariableLocation), Offset) -> SRSS (M.Map String (RegisterName, VariableLocation), Offset)
foldLocal spilled rMapping var (mapping, offset) = do
    info <- lift (gets (getSymbolInfo var))
    let newoffset = case info of
            Nothing -> offset - 4
            (Just x) -> let t = infoType x
                            kind = infoKind x
                            size = infoSize x
                        in offset - (if t == IntType || kind == Pointer then 4 else 1)
    if var `elem` spilled
        then return (M.insert var (rMapping M.! var, InStack newoffset) mapping, newoffset)
    else return (M.insert var (rMapping M.! var, InRegister (rMapping M.! var)) mapping, offset)

foldParam :: M.Map String RegisterName -> Variable -> (M.Map String (RegisterName, VariableLocation), Offset) -> SRSS (M.Map String (RegisterName, VariableLocation), Offset)
foldParam rMapping var (mapping, offset) = do
    info <- lift (gets (unsafeGetSymbolInfo var))
    let t = infoType info
    let kind = infoKind info
    let newoffset = offset + (if t == IntType || kind == Pointer then 4 else 1)
    return (M.insert var (rMapping M.! var, InStack offset) mapping, newoffset)

modifyInstructions :: [Variable] -> [Variable] -> [Variable] -> TACFunction -> TACFunction
modifyInstructions registers params globals is = 
    let paramsToLoad = registers `intersect` params
        globalsToLoad = registers `intersect` globals
    in map TACLoad paramsToLoad ++ fixInstructions is globalsToLoad

instance NASMGenerator TACFunction where
    nasmGenerateInstructions xs = do
        st <- lift get
        case nextDF st of
            Nothing -> lift $ put st
            (Just x) -> lift $ put x
        let funcName = (labelToName . head) xs
        let negConstraints = (constraintsRegisterNameToInt . negativeConstraints) xs
        let (varIntMap, spilled, is) = mapVariablesToRegisters xs (length registers) negConstraints
        let varRegMap = M.map ((!!) registers) varIntMap
        let variables = M.keys varRegMap
        let inRegisters = variables \\ spilled
        st <- lift get
        params <- lift (gets (map fst . infoParams . (unsafeGetSymbolInfo funcName)))
        let globals = filter (flip nameInParent st) variables
        let locals = (variables \\ globals) \\ params
        (localMapping, offset) <- foldrM (foldLocal spilled varRegMap) (M.empty, 0) locals
        (paramMapping, _) <- foldrM (foldParam varRegMap) (M.empty, 8) (reverse params)
        let globalMapping = M.fromList $ map (\x -> (x, (varRegMap M.! x, InMemory x))) globals
        let totalMapping = M.unions $ ([paramMapping, localMapping, globalMapping])
        put totalMapping
        pre <- nasmGeneratePreFunction funcName offset
        nasmIS <- (mapM nasmGenerateInstructions (modifyInstructions inRegisters params globals (tail is))) >>= return . concat
        post <- nasmGeneratePostFunction funcName
        return $ pre ++ nasmIS


retInstructions :: [NASMInstruction]
retInstructions = [POP1 DI, POP1 SI, POP1 B, MOV1 DWORD SP BP, POP1 BP, RET]

exitInstructions :: [NASMInstruction]
exitInstructions = [CALL "_exit"]

varRegister :: Variable -> SRSS RegisterName
varRegister var = gets (fst . (M.! var))

varLocation :: Variable -> SRSS VariableLocation
varLocation var = gets (snd . (M.! var))

pointerDeRef :: String -> SRSS Address
pointerDeRef var = do
    reg <- varRegister var
    return $ AddressRegisterOffset (Register reg DWORD) 0 1

varAddress :: String -> SRSS Address
varAddress var = do
    loc <- varLocation var
    case loc of
         (InStack offset) -> return $ AddressRegisterOffset (Register BP DWORD) offset 1
         (InMemory label) -> return $ AddressLabelOffset label 0 1

arrayAddress :: String -> TACExpression -> SRSS Address
arrayAddress var ex = do
    reg <- varRegister var
    vartype <- lift $ gets (infoType . (unsafeGetSymbolInfo var))
    let multiplier = if vartype == IntType then 4 else 1
    case ex of
         (TACInt i) -> return $ AddressRegisterOffset (Register reg DWORD) i multiplier
         (TACChar c) -> return $ AddressRegisterOffset (Register reg DWORD) (ord c) multiplier
         (TACVar v) -> varRegister v >>= \r2 -> return $ AddressRegisterRegister (Register reg DWORD) 0 (Register r2 DWORD) multiplier

nasmGenerateMove :: Variable -> TACExpression -> SRSS [NASMInstruction]
nasmGenerateMove var ex = do
    info <- gets (M.lookup var)
    case info of 
        Nothing -> return []
        (Just (dest, _)) -> 
            case ex of
                 (TACInt i) -> return [MOV4 (Register dest DWORD) i]
                 (TACChar c) -> return [MOV4 (Register dest LSB) (ord c)]
                 (TACVar v) -> varRegister v >>= \reg -> if dest /= reg 
                                                                    then return [MOV1 DWORD dest reg]
                                                                else return []

nasmWrapNoRegister :: [RegisterName] -> [RegisterName -> NASMInstruction] -> [NASMInstruction]
nasmWrapNoRegister toKeep inst = let pick = head [x | x <- registers, not (x `elem` toKeep)]
                                 in [PUSH1 pick] ++ map ($ pick) inst ++ [POP1 pick]

nasmGeneratePlus :: Variable -> TACExpression -> TACExpression -> SRSS [NASMInstruction]
nasmGeneratePlus var e1 e2 = do
    dest <- varRegister var
    mov1 <- nasmGenerateMove var e1
    case e1 of
         (TACInt i1) -> case e2 of
                            (TACInt i2) -> return [MOV4 (Register dest DWORD) i1, ADD4 (Register dest DWORD) i2]
                            (TACChar c2) -> return [MOV4 (Register dest DWORD) i1, ADD4 (Register dest DWORD) (ord c2)]
                            (TACVar v2) -> if v2 == var then return [ADD4 (Register dest DWORD) i1]
                                           else varRegister v2 >>= \reg -> return [MOV4 (Register dest DWORD) i1, ADD1 DWORD dest reg]
         (TACChar c1) -> case e2 of
                            (TACInt i2) -> return [MOV4 (Register dest DWORD) (ord c1), ADD4 (Register dest DWORD) i2]
                            (TACChar c2) -> return [MOV4 (Register dest DWORD) (ord c1), ADD4 (Register dest DWORD) (ord c2)]
                            (TACVar v2) -> if v2 == var then return [ADD4 (Register dest DWORD) (ord c1)]
                                           else varRegister v2 >>= \reg -> return [MOV4 (Register dest DWORD) (ord c1), ADD1 DWORD dest reg]

         (TACVar v1) -> varRegister v1 >>= \r1 -> case e2 of
                            (TACInt i2) -> return $ mov1 ++ [ADD4 (Register dest DWORD) i2]
                            (TACChar c2) -> return $ mov1 ++ [ADD4 (Register dest DWORD) (ord c2)]
                            (TACVar v2) -> varRegister v2 >>= \r2 -> return $ if r1 == dest then [ADD1 DWORD dest r2] else if r2 == dest then [ADD1 DWORD dest r1] else mov1 ++ [ADD1 DWORD dest r2]

nasmGenerateMinus :: Variable -> TACExpression -> TACExpression -> SRSS [NASMInstruction]
nasmGenerateMinus var e1 e2 = do
    dest <- varRegister var
    mov1 <- nasmGenerateMove var e1
    case e1 of
         (TACInt i1) -> case e2 of
                            (TACInt i2) -> return [MOV4 (Register dest DWORD) i1, SUB4 (Register dest DWORD) i2]
                            (TACChar c2) -> return [MOV4 (Register dest DWORD) i1, SUB4 (Register dest DWORD) (ord c2)]
                            (TACVar v2) -> if v2 == var then return [SUB4 (Register dest DWORD) i1]
                                           else varRegister v2 >>= \reg -> return [MOV4 (Register dest DWORD) i1, SUB1 DWORD dest reg]
         (TACChar c1) -> case e2 of
                            (TACInt i2) -> return [MOV4 (Register dest DWORD) (ord c1), SUB4 (Register dest DWORD) i2]
                            (TACChar c2) -> return [MOV4 (Register dest DWORD) (ord c1), SUB4 (Register dest DWORD) (ord c2)]
                            (TACVar v2) -> if v2 == var then return [SUB4 (Register dest DWORD) (ord c1)]
                                           else varRegister v2 >>= \reg -> return [MOV4 (Register dest DWORD) (ord c1), SUB1 DWORD dest reg]

         (TACVar v1) -> varRegister v1 >>= \r1 -> case e2 of
                            (TACInt i2) -> return $ mov1 ++ [SUB4 (Register dest DWORD) i2]
                            (TACChar c2) -> return $ mov1 ++ [SUB4 (Register dest DWORD) (ord c2)]
                            (TACVar v2) -> varRegister v2 >>= \r2 -> return $ if r1 == dest then [SUB1 DWORD dest r2] else if r2 == dest then [SUB1 DWORD dest r1, NEG1 (Register dest DWORD)] else mov1 ++ [SUB1 DWORD dest r2]

nasmGenerateTimes :: Variable -> TACExpression -> TACExpression -> SRSS [NASMInstruction]
nasmGenerateTimes var e1 e2 = do
    dest <- varRegister var
    mov <- nasmGenerateMove var e1
    case e1 of
         (TACInt i1) -> case e2 of
                             (TACInt i2) -> return $ mov ++ [IMUL3 dest i2]
                             (TACChar c2) -> return $ mov ++ [IMUL3 dest (ord c2)]
                             (TACVar v2) -> if v2 == var then return [IMUL3 dest i1]
                                            else varRegister v2 >>= \src -> return [IMUL6 dest src i1]
         (TACChar c1) -> case e2 of
                             (TACInt i2) -> return [MOV4 (Register dest DWORD) (ord c1), IMUL3 dest i2]
                             (TACChar c2) -> return [MOV4 (Register dest DWORD) (ord c1), IMUL3 dest (ord c2)]
                             (TACVar v2) -> if v2 == var then return [IMUL3 dest (ord c1)]
                                            else varRegister v2 >>= \src -> return [IMUL6 dest src (ord c1)]
         (TACVar v1) -> varRegister v1 >>= \src1 -> 
                            case e2 of
                                (TACInt i2) -> if src1 == dest then return [IMUL3 dest i2]
                                               else return [IMUL6 dest src1 i2]
                                (TACChar c2) -> if src1 == dest then return [IMUL3 dest (ord c2)]
                                                else return [IMUL6 dest src1 (ord c2)]
                                (TACVar v2) -> varRegister v2 >>= \src2 -> 
                                                    if src1 == dest then return [IMUL4 dest src2]
                                                    else if src2 == dest then return [IMUL4 dest src1]
                                                    else return $ mov ++ [IMUL4 dest src2]


nasmGenerateDivide :: Variable -> TACExpression -> TACExpression -> SRSS [NASMInstruction]
nasmGenerateDivide var e1 e2 = do
    dest <- varRegister var
    let predest = if dest == A then [PUSH1 D, XOR1 DWORD D D] 
                  else if dest == D then [PUSH1 A, XOR1 DWORD D D] 
                  else [PUSH1 D, XOR1 DWORD D D, PUSH1 A]
    let pre src = if src == A then [] else [MOV1 DWORD A src]
    let postdest = if dest == A then [POP1 D]
                   else if dest == D then [MOV1 DWORD dest A, POP1 A]
                   else [MOV1 DWORD dest A, POP1 A, POP1 D]
    case e1 of
         (TACInt i1) -> case e2 of
                             (TACInt i2) -> return $ predest ++ [PUSH1 B, MOV4 (Register A DWORD) i1, MOV4 (Register B DWORD) i2, IDIV1 B, POP1 B] ++ postdest
                             (TACChar c2) -> return $ predest ++ [PUSH1 B, MOV4 (Register A DWORD) i1, MOV4 (Register B DWORD) (ord c2), IDIV1 B, POP1 B] ++ postdest
                             (TACVar v2) -> varRegister v2 >>= \src -> return $ predest ++ [MOV4 (Register A DWORD) i1, IDIV1 src] ++ postdest
         (TACChar c1) -> case e2 of
                             (TACInt i2) -> return $ predest ++ [PUSH1 B, MOV4 (Register A DWORD) (ord c1), MOV4 (Register B DWORD) i2, IDIV1 B, POP1 B] ++ postdest
                             (TACChar c2) -> return $ predest ++ [PUSH1 B, MOV4 (Register A DWORD) (ord c1), MOV4 (Register B DWORD) (ord c2), IDIV1 B, POP1 B] ++ postdest
                             (TACVar v2) -> varRegister v2 >>= \src -> return $ predest ++ [MOV4 (Register A DWORD) (ord c1), IDIV1 src] ++ postdest
         (TACVar v1) -> case e2 of
                             (TACInt i2) -> varRegister v1 >>= \src -> return $ predest ++ pre src ++ nasmWrapNoRegister [src, A, D] [\x -> MOV4 (Register x DWORD) i2, \x -> IDIV1 x] ++ postdest
                             (TACChar c2) -> varRegister v1 >>= \src -> return $ predest ++ pre src ++ nasmWrapNoRegister [src, A, D] [\x -> MOV4 (Register x DWORD) (ord c2), \x -> IDIV1 x] ++ postdest
                             (TACVar v2) -> varRegister v1 >>= \src1 -> varRegister v2 >>= \src2 -> return $ predest ++ pre src1 ++ [IDIV1 src2] ++ postdest


nasmCall :: Label -> [TACExpression] -> (Maybe Variable) -> SRSS [NASMInstruction]
nasmCall label args ret = do
    let f arg = case arg of
                     (TACInt i) -> return $ PUSH3 i
                     (TACChar c) -> return $ PUSH3 (ord c)
                     (TACVar v) -> varRegister v >>= \reg -> return $ PUSH1 reg
    pushargs <- mapM f (reverse args)
    let popargs = [ADD4 (Register SP DWORD) (length args * 4)]
    (saveregs, restoreregs) <- case ret of
                        Nothing -> return (map PUSH1 [A, C, D], map POP1 [D, C, A])
                        (Just x) -> varRegister x >>= \reg -> return (((map PUSH1) . (filter ((/=) reg))) [A, C, D], [MOV1 DWORD reg A] ++ ((map POP1) . filter ((/=) reg)) [D, C, A])
    return $ saveregs ++ pushargs ++ [CALL label] ++ popargs ++ restoreregs

instance NASMGenerator TACInstruction where
    nasmGenerateInstructions (TACBinary var e1 op e2) =
        case op of
             TACPlus -> nasmGeneratePlus var e1 e2
             TACMinus -> nasmGenerateMinus var e1 e2
             TACTimes -> nasmGenerateTimes var e1 e2
             TACDivide -> nasmGenerateDivide var e1 e2
    nasmGenerateInstructions (TACUnary var op ex) = do
        let applyOp = varRegister var >>= \reg -> case op of
                                                       TACNeg -> return [NEG1 (Register reg DWORD)]
                                                       TACNot -> return [NOT1 (Register reg DWORD)]
        liftM2 (++) (nasmGenerateMove var ex) applyOp
    nasmGenerateInstructions (TACCopy var ex) = nasmGenerateMove var ex
    nasmGenerateInstructions (TACLoad var) = do
        reg <- varRegister var
        location <- varLocation var
        case location of
             (InStack offset) -> return [MOV2 (Register reg DWORD) (AddressRegisterOffset (Register BP DWORD) offset 1)]
             (InMemory label) -> do
                info <- lift (gets (unsafeGetSymbolInfo var))
                let k = infoKind info
                if k == Array then return [LEA reg (AddressLabelOffset label 0 1)]
                else return [MOV2 (Register reg DWORD) (AddressLabelOffset label 0 1)]
    nasmGenerateInstructions (TACStore var) = do
        reg <- varRegister var
        location <- varLocation var
        let addr = case location of
                        (InStack offset) -> AddressRegisterOffset (Register BP DWORD) offset 1
                        (InMemory label) -> AddressLabelOffset label 0 1
        return [MOV3 addr (Register reg DWORD)]
    nasmGenerateInstructions (TACIf (TACExpr e1 op e2) label) = do
        cmp <- case e1 of
                    (TACInt i1) -> case e2 of
                                        (TACInt i2) -> return [PUSH1 A, MOV4 (Register A DWORD) i1, CMP4 (Register A DWORD) i2, POP1 A]
                                        (TACChar c2) -> return [PUSH1 A, MOV4 (Register A DWORD) i1, CMP4 (Register A DWORD) (ord c2), POP1 A]
                                        (TACVar v2) -> varRegister v2 >>= \r2 -> return [PUSH1 A, MOV4 (Register A DWORD) i1, CMP1 (Register A DWORD) (Register r2 DWORD), POP1 A]
                    (TACChar c1) -> case e2 of
                                        (TACInt i2) -> return [PUSH1 A, MOV4 (Register A DWORD) (ord c1), CMP4 (Register A DWORD) i2, POP1 A]
                                        (TACChar c2) -> return [PUSH1 A, MOV4 (Register A DWORD) (ord c1), CMP4 (Register A DWORD) (ord c2), POP1 A]
                                        (TACVar v2) -> varRegister v2 >>= \r2 -> return [PUSH1 A, MOV4 (Register A DWORD) (ord c1), CMP1 (Register A DWORD) (Register r2 DWORD), POP1 A]
                    (TACVar v1) -> varRegister v1 >>= \r1 -> case e2 of
                                                                  (TACInt i2) -> return [CMP4 (Register r1 DWORD) i2]
                                                                  (TACChar c2) -> return [CMP4 (Register r1 DWORD) (ord c2)]
                                                                  (TACVar v2) -> varRegister v2 >>= \r2 -> return [CMP1 (Register r1 DWORD) (Register r2 DWORD)]
        let jmp = case op of
                       TACEqual -> [JE label]
                       TACNotEqual -> [JNE label]
                       TACGreater -> [JG label]
                       TACLess -> [JL label]
        return $ cmp ++ jmp

    nasmGenerateInstructions (TACIf _ _) = error "This TACIf is not implemented"
    nasmGenerateInstructions (TACGoto label) = return [JMP label]
    nasmGenerateInstructions (TACCall label args ret) = do
        case ret of 
             Nothing -> nasmCall label args Nothing
             Just x -> case x of
                            (TACVar v) -> nasmCall label args (Just v)
                            _ -> error $ "Return expression " ++ show ret ++ " is not a variable in " ++ show (TACCall label args ret)
    nasmGenerateInstructions (TACReturn Nothing) = do
        (Flags tiny) <- lift $ lift get
        if tiny then return exitInstructions
        else return retInstructions
    nasmGenerateInstructions (TACReturn (Just ex)) = do
        mapping <- get
        (Flags tiny) <- lift $ lift get
        let retValue = case ex of
                            (TACInt i) -> [MOV4 (Register A DWORD) i]
                            (TACChar c) -> [MOV4 (Register A DWORD) (ord c)]
                            (TACVar var) -> 
                                let reg = fst (mapping M.! var) 
                                in  if reg == A then []
                                    else [MOV1 DWORD A reg]
        let ret = if tiny then exitInstructions else retInstructions
        return $ retValue ++ ret
    nasmGenerateInstructions (TACLabel label) = return [LABEL label]
    nasmGenerateInstructions (TACWrite typ ex) = 
        case ex of
            (TACVar x) -> if typ == CharType then nasmCall "_writechar" [ex] Nothing
                          else nasmCall "_writeint" [ex] Nothing
            (TACChar c) -> nasmCall "_writechar" [ex] Nothing
            (TACInt i) -> nasmCall "_writeint" [ex] Nothing
    nasmGenerateInstructions (TACRead typ ex) = 
        case ex of
            (TACVar x) -> if typ == CharType then nasmCall "_read_char" [] (Just x)
                          else nasmCall "_read_int" [] (Just x)
    nasmGenerateInstructions (TACArrayDecl var es) = do
        reg <- varRegister var
        info <- lift (gets (unsafeGetSymbolInfo var))
        let mult = if infoType info == IntType then 4 else 1
        let ArraySize size = infoSize info
        return [SUB4 (Register SP DWORD) (mult * (product size)), MOV1 DWORD reg SP]
    nasmGenerateInstructions (TACArrayAccess var array index) = do
        reg <- varRegister var
        t <- lift $ gets (infoType . (unsafeGetSymbolInfo array))
        addr <- arrayAddress array index
        if t == IntType then return [MOV2 (Register reg DWORD) addr]
        else return [XOR1 DWORD reg reg, MOV2 (Register reg LSB) addr]
    nasmGenerateInstructions (TACArrayModif array index ex) = do
        addr <- arrayAddress array index
        t <- lift $ gets (infoType . (unsafeGetSymbolInfo array))
        let size = if t == IntType then DWORDADDRESS else BYTEADDRESS
        case ex of
             (TACInt i) -> return [MOV5 size addr i]
             (TACChar c) -> return [MOV5 size addr (ord c)]
             (TACVar v) -> varRegister v >>= \reg -> return [MOV3 addr (Register reg DWORD)]
    nasmGenerateInstructions (TACAddress var ex) = do
        reg <- varRegister var
        case ex of
            (TACVar name) -> varAddress name >>= \addr -> return [LEA reg addr]
            (TACArray name index) -> arrayAddress name index >>= \addr -> return [LEA reg addr]
            _ -> return []
    nasmGenerateInstructions (TACDeRef var ex) = do
        reg <- varRegister var
        case ex of
            (TACVar v2) -> pointerDeRef v2 >>= \addr -> return [MOV2 (Register reg DWORD) addr]
    nasmGenerateInstructions (TACDeRefA var ex) = do
        info <- gets (M.lookup var)
        case info of
            Nothing -> return []
            Just _ -> pointerDeRef var >>= \addr -> 
                            case ex of
                                (TACInt i2) -> return [MOV5 DWORDADDRESS addr i2]
                                (TACChar c2) -> return [MOV5 BYTEADDRESS addr (ord c2)]
                                (TACVar v2) -> varRegister v2 >>= \reg -> return [MOV3 addr (Register reg DWORD)]
type SRSS = StateT RegisterState (StateT SymbolTable (State Flags))

data Flags = Flags {
    inTiny :: Bool
}

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
                     | MOV4 Register Constant
                     | MOV5 AddressSize Address Constant
                     | XCHG1 RegisterName -- Exchange register with eax
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
                     | NEG1 Register
                     | NEG2 Address
                     | IMUL1 Register -- Multiplied by A (depending on size) and stored in A
                     | IMUL2 AddressSize Address
                     | IMUL3 RegisterName Constant -- Only 32-bit register
                     | IMUL4 RegisterName RegisterName -- Only 32-bit register
                     | IMUL5 RegisterName Address -- Only 32-bit register
                     | IMUL6 RegisterName RegisterName Constant -- Only 32-bit register
                     | IMUL7 RegisterName Address Constant -- Only 32-bit register
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

class NASMShow a where
    nasmShow :: a -> String

instance NASMShow a => NASMShow [a] where
    nasmShow = concatMap (\x -> nasmShow x ++ "\n")

instance NASMShow String where
    nasmShow = (tail . init . show)

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
        "mov ebx, 0"
        "int 0x80"
        "_writechar:"
        "push ebp"
        "mov ebp, esp"
        "push ebx"
        "push esi"
        "push edi"
        "mov edx,1     ; arg3, length of string to print"
        "lea ecx,[ebp+8]     ; arg2, pointer to string"
        "mov ebx,1       ; arg1, where to write, screen"
        "mov eax,4       ; write sysout command to int 80 hex"
        "int 0x80        ; interrupt 80 hex, call kernel"
        "pop edi"
        "pop esi"
        "pop ebx"
        "mov esp, ebp"
        "pop ebp"
        "ret"
        "_writeint:"
        "push ebp"
        "mov ebp, esp"
        "push ebx"
        "push esi"
        "push edi"
        "mov eax, [ebp+8] ; arg 1 : the int"
        "mov ecx, 0      ; byte counter"
        "mov ebx, 10      ; base 10"
        "; check if positive or negative"
        "test eax, eax"
        "jns _writeint_beg"
        "neg eax"
        "_writeint_beg:"
        "xor edx, edx"
        "idiv ebx"
        "add edx, 48"
        "sub esp, 1"
        "mov [esp], dl"
        "add ecx, 1"
        "cmp eax, 0"
        "jne _writeint_beg"
        "; add - if negative"
        "mov eax, [ebp+8]"
        "test eax, eax"
        "jns _write_not_signed"
        "mov bl, 45"
        "sub esp, 1"
        "mov [esp], bl"
        "add ecx, 1"
        "_write_not_signed:"
        "mov esi, ecx"
        "mov edx,ecx     ; arg3, length of string to print"
        "mov ecx,esp     ; arg2, pointer to string"
        "mov ebx,1       ; arg1, where to write, screen"
        "mov eax,4       ; write sysout command to int 80 hex"
        "int 0x80        ; interrupt 80 hex, call kernel"
        "add esp, esi"
        "pop edi"
        "pop esi"
        "pop ebx"
        "mov esp, ebp"
        "pop ebp"
        "ret"
        "_read_char:"
        "push ebp"
        "mov ebp, esp"
        "push ebx"
        "push esi"
        "push edi"
        "sub esp, 1"
        "mov edx,1     ; arg3, length of string to read"
        "lea ecx,[esp]     ; arg2, pointer to string"
        "mov ebx,0       ; arg1, where to read, keyboard"
        "mov eax,3      ; write sysin command to int 80 hex"
        "int 0x80        ; interrupt 80 hex, call kernel"
        "movzx eax, byte [esp]"
        "add esp, 1"
        "pop edi"
        "pop esi"
        "pop ebx"
        "mov esp, ebp"
        "pop ebp"
        "ret"
        "_read_int:"
        "push ebp"
        "mov ebp, esp"
        "push ebx"
        "push esi"
        "push edi"
        "sub esp, 100"
        "mov edx,100     ; arg3, length of string to read"
        "mov ecx,esp     ; arg2, pointer to string"
        "mov ebx,0       ; arg1, where to read, keyboard"
        "mov eax,3      ; write sysin command to int 80 hex"
        "int 0x80        ; interrupt 80 hex, call kernel"
        "sub eax, 1"
        "mov ecx, 0" -- counter
        "mov ebx, 0" -- result
        "cmp byte [esp], 45" -- if first char is - ignore it for the moment
        "jne _read_int_beg"
        "add ecx, 1"
        "_read_int_beg:" -- start of loop
        "cmp ecx, eax" -- end if counter is higher than total number of chars
        "jge _read_int_end"
        "imul ebx, 10"
        "movzx esi, byte [esp+ecx]"
        "sub esi, 48"
        "add ebx, esi"
        "add ecx, 1"
        "jmp _read_int_beg"
        "_read_int_end:"
        "cmp byte [esp], 45"
        "jne _read_int_not_signed"
        "neg ebx"
        "_read_int_not_signed:"
        "mov eax, ebx"
        "add esp, 100"
        "pop edi"
        "pop esi"
        "pop ebx"
        "mov esp, ebp"
        "pop ebp"
        "ret"

 
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
    nasmShow (XCHG1 reg) = instShow "xchg" [nasmShow reg, nasmShow (Register A DWORD)]
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
    nasmShow (NEG1 reg) = instShow "neg" [nasmShow reg]
    nasmShow (NEG2 addr) = instShow "neg" [nasmShow addr]
    nasmShow (IMUL1 reg) = instShow "imul" [nasmShow reg]
    nasmShow (IMUL2 size addr) = "imul " ++ nasmShow size ++ " " ++ nasmShow addr
    nasmShow (IMUL3 r1 cst) = instShow "imul" [nasmShow (Register r1 DWORD), nasmShow cst]
    nasmShow (IMUL4 r1 r2) = instShow "imul" [nasmShow (Register r1 DWORD), nasmShow (Register r2 DWORD)]
    nasmShow (IMUL5 rname addr) = instShow "imul" [nasmShow (Register rname DWORD), nasmShow addr]
    nasmShow (IMUL6 r1 r2 cst) = instShow "imul" [nasmShow (Register r1 DWORD), nasmShow (Register r2 DWORD), nasmShow cst]
    nasmShow (IMUL7 rname addr cst) = instShow "imul" [nasmShow (Register rname DWORD), nasmShow addr, nasmShow cst]
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
    nasmShow BYTEADDRESS = "byte"
    nasmShow WORDADDRESS = "word"
    nasmShow DWORDADDRESS = "dword"

showOffsetMult :: Offset -> Multiplier -> String
showOffsetMult offset mult = if offset /= 0 && mult /= 0 then 
                                 "+" ++ if mult /= 1 then nasmShow offset ++ "*" ++ nasmShow mult
                                        else nasmShow offset
                             else ""

showOffsetRegisterMult :: Offset -> Register -> Multiplier -> String
showOffsetRegisterMult offset reg mult = (if offset /= 0 then "+" ++ nasmShow offset else "")
                                      ++ (if mult /= 0 then "+" ++ nasmShow reg ++ 
                                              (if mult /= 1 then "*" ++ show mult else "")
                                          else "")

instance NASMShow Address where
    nasmShow (AddressRegisterOffset reg offset mult) = "[" ++ nasmShow reg ++ showOffsetMult offset mult ++ "]"
    nasmShow (AddressRegisterRegister r1 offset r2 mult) = "[" ++ nasmShow r1 ++ showOffsetRegisterMult offset r2 mult ++ "]"
    nasmShow (AddressLabelOffset label offset mult) = "[" ++ nasmShow label ++ showOffsetMult offset mult ++ "]"
    nasmShow (AddressLabelRegister label register mult) = "[" ++ nasmShow label ++ showOffsetRegisterMult 0 register mult ++ "]"
instance NASMShow Multiplier where
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
