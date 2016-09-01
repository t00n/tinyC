{-# LANGUAGE FlexibleInstances #-}

module NASMGenerator (nasmGenerate, nasmGenerateData, nasmGenerateText) where

import qualified Data.Set as S
import qualified Data.Map as M
import Control.Monad.State
import Data.Char (ord)
import Debug.Trace (traceShow)
import Data.Maybe (fromJust)
import Data.Foldable (foldrM)
import Data.List (intersect, (\\))


import TACProgram
import SymbolTable
import TACAnalysis
import Graph
import AST
import NASMAnalysis
import NASMProgram

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
varRegister var = gets (fst . flip (M.!) var)

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