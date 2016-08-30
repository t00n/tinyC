module TACGenerator (tacGenerate, TACProgram(..), TACFunction(..), TACInstruction(..), TACBinaryOperator(..), TACUnaryOperator(..), TACExpression(..), TACPrint(..), tacData, tacCode) where

import Control.Monad.Trans.State (StateT(..), evalStateT, gets, get, put)
import Control.Monad.Trans (lift)
import Control.Monad (liftM2)
import Data.Char (chr, ord)
import Data.Sequence (foldrWithIndex)
import Debug.Trace (traceShow)

import Parser
import MonadNames
import Utility
import SymbolTable

import Data.List(intercalate)

type SSNSS = StateT SymbolTable (Names String String)

infiniteNames :: String -> [String]
infiniteNames s = [s ++ show i | i <- [1..]]

tacGenerate :: SymbolTable -> [Declaration] -> TACProgram
tacGenerate st = TACProgram <$> tacGenerateData st <*> tacGenerateText st

tacGenerateData :: SymbolTable -> [Declaration] -> [TACInstruction]
tacGenerateData st xs = evalNames (evalStateT (tacGenerateInstructions xs) st) (infiniteNames "t") (infiniteNames "l")

tacGenerateText :: SymbolTable -> [Declaration] -> [[TACInstruction]]
tacGenerateText st xs = evalNames (evalStateT (tacGenerateAllFunctions xs) st) (infiniteNames "t") (infiniteNames "l")

class TACGenerator a where
    tacGenerateInstructions :: a -> SSNSS [TACInstruction]

instance TACGenerator a => TACGenerator [a] where
    tacGenerateInstructions [] = return []
    tacGenerateInstructions (x:xs) = liftM2 (++) (tacGenerateInstructions x) (tacGenerateInstructions xs)

tacGenerateAllFunctions :: [Declaration] -> SSNSS [[TACInstruction]]
tacGenerateAllFunctions [] = return []
tacGenerateAllFunctions (x:xs) =
    case x of
        FuncDeclaration _ _ _ _ -> liftM2 (:) (tacGenerateFunction x) (tacGenerateAllFunctions xs)
        _ -> tacGenerateAllFunctions xs

tacGenerateFunction :: Declaration -> SSNSS [TACInstruction]
tacGenerateFunction (FuncDeclaration t name params stmt) = do
        st <- get
        case nextDF st of
            Nothing -> put st
            (Just x) -> put x
        functionBody <- tacGenerateInstructions stmt
        let ret = if functionBody == [] then [TACReturn Nothing] 
            else case last functionBody of
                (TACReturn _) -> []
                _ -> [TACReturn Nothing]
        return $ TACLabel (nameToString name) : functionBody ++ ret

instance TACGenerator Declaration where
    tacGenerateInstructions (VarDeclaration t (Name n) Nothing) = 
        let value = if t == IntType then TACInt 0 else TACChar (chr 0)
        in return $ [TACCopy n value]
    tacGenerateInstructions (VarDeclaration _ (Name n) (Just e)) = do
        (_, t, lines) <- tacExpression e
        return $ lines ++ [TACCopy n t]
    tacGenerateInstructions (VarDeclaration t n@(NameSubscription _ _) _) = do
        let name = nameToString n
        let value = if t == IntType then TACInt 0 else TACChar (chr 0)
        ArraySize size <- gets $ unsafeSymbolSize name
        return $ [TACArrayDecl name (replicate (product size) value)]
    tacGenerateInstructions (VarDeclaration t (NamePointer n) Nothing) = 
        return [TACCopy n (TACInt 0)]
    tacGenerateInstructions (VarDeclaration t (NamePointer n) (Just e)) = do
        (_, t, lines) <- tacExpression e
        return $ lines ++ [TACCopy n t]
    tacGenerateInstructions _ = return []

tacArraySize :: [Expression] -> String -> SSNSS Expression
tacArraySize es var = do
    size <- gets (unsafeSymbolSize var)
    case size of
        VarSize s -> return $ es !! 0
        ArraySize ss -> return $ foldr1 (\x acc -> BinOp x Plus acc) $ map (\(i, s) -> BinOp i Times (Int s)) (zip es (tail ss ++ [1]))


instance TACGenerator Statement where
    tacGenerateInstructions (Assignment (Name n) e) = do
        (_, t, lines) <- tacExpression e
        return $ lines ++ [TACCopy n t]
    tacGenerateInstructions (Assignment (NameSubscription name indexes) e) = do
        ex <- tacArraySize indexes name
        (_, t1, lines1) <- tacExpression ex
        (_, t2, lines2) <- tacExpression e
        return $ lines1 ++ lines2 ++ [TACArrayModif name t1 t2]
    tacGenerateInstructions (Assignment (NamePointer n) e) = do
        (_, t, lines) <- tacExpression e
        return $ lines ++ [TACDeRefA n t]
    tacGenerateInstructions (If e stmt) = do
        (t, lines) <- tacRelExpression e
        labelYes <- lift popLabel
        labelNo <- lift popLabel
        stmt <- tacGenerateInstructions stmt
        return $ lines ++ [TACIf t labelYes, TACGoto labelNo, TACLabel labelYes] ++ stmt ++ [TACLabel labelNo]
    tacGenerateInstructions (IfElse e s1 s2) = do
        (t, lines) <- tacRelExpression e
        labelYes <- lift popLabel
        labelNo <- lift popLabel
        labelEnd <- lift popLabel
        s1 <- tacGenerateInstructions s1
        s2 <- tacGenerateInstructions s2
        return $ lines ++ [TACIf t labelYes, TACGoto labelNo, TACLabel labelYes] ++ s1 ++ [TACGoto labelEnd, TACLabel labelNo] ++ s2 ++ [TACLabel labelEnd]
    tacGenerateInstructions (While e s) = do
        (t, lines) <- tacRelExpression e
        labelBeg <- lift popLabel
        labelEnd <- lift popLabel
        labelYes <- lift popLabel
        s <- tacGenerateInstructions s
        return $ [TACLabel labelBeg] ++ lines ++ [TACIf t labelYes, TACGoto labelEnd, TACLabel labelYes] ++ s ++ [TACGoto labelBeg, TACLabel labelEnd]
    tacGenerateInstructions (Return e) = do
        (_, t, lines) <- tacExpression e
        return $ lines ++ [TACReturn $ Just t]
    tacGenerateInstructions (Block ds ss) = do
        ds <- tacGenerateInstructions ds
        ss <- tacGenerateInstructions ss
        return $ ds ++ ss
    tacGenerateInstructions (Write e) = do
        (typ, t, lines) <- tacExpression e
        return $ lines ++ [TACWrite typ t]
    tacGenerateInstructions (Read n) = do
        (typ, t, lines) <- tacExpression (Var n)
        return $ lines ++ [TACRead typ t]
    tacGenerateInstructions (Expr e) = do
        (_, t, lines) <- tacExpression e
        let newlines = if lines /= [] then
                            case last lines of
                                (TACCall label args _) -> init lines ++ [TACCall label args Nothing]
                                _ -> lines
                        else lines
        return newlines

tacRelExpression :: Expression -> SSNSS (TACExpression, [TACInstruction])
tacRelExpression ex@(BinOp e1 op e2) = 
    let isRelOp Equal = True
        isRelOp NotEqual = True
        isRelOp Greater = True
        isRelOp Less = True
        isRelOp _ = False
    in
    case isRelOp op of
        False -> do
            (_, t, lines) <- tacExpression ex
            return (TACExpr t TACNotEqual (TACInt 0), lines)
        True -> do
            (_, t1, lines1) <- tacExpression e1
            (_, t2, lines2) <- tacExpression e2
            return (TACExpr t1 (tacBinaryOperator op) t2, lines1 ++ lines2)
tacRelExpression (Int i) = return (TACExpr (TACInt i) TACNotEqual (TACInt 0), [])
tacRelExpression (Char c) = return (TACExpr (TACChar c) TACNotEqual (TACInt 0), [])
tacRelExpression (UnOp op e) = 
    case op of
        Neg -> tacRelExpression e
        Not -> do
            (_, t, lines) <- tacExpression e
            return (TACExpr t TACEqual (TACInt 0), lines)
tacRelExpression (Var v) = do
    (_, t, lines) <- tacExpression (Var v)
    return (TACExpr t TACNotEqual (TACInt 0), lines)
tacRelExpression e = do (_, b, c) <- tacExpression e; return (b, c)

tacExpression :: Expression -> SSNSS (Type, TACExpression, [TACInstruction])
tacExpression (Int i) = return (IntType, TACInt i, [])
tacExpression (Char i) = return (CharType, TACChar i, [])
tacExpression (BinOp e1 op e2) = do
    (typ1, t1, lines1) <- tacExpression e1
    (typ2, t2, lines2) <- tacExpression e2
    let typ = if typ1 == CharType && typ2 == CharType then CharType else IntType
    var <- lift popVariable
    let newline = TACBinary var t1 (tacBinaryOperator op) t2
    return (typ, TACVar var, lines1 ++ lines2 ++ [newline])           
tacExpression (UnOp op e) = do
    (typ, t, lines) <- tacExpression e
    var <- lift popVariable
    let newline = TACUnary var (tacUnaryOperator op) t
    return (typ, TACVar var, lines ++ [newline])
tacExpression (Call n es) = do
    reducedES <- mapM tacExpression es
    let params = map (\(_, b, c) -> b) reducedES
    let lines = concatMap (\(_, b, c) -> c) reducedES
    t <- lift popVariable
    typ <- gets (unsafeSymbolType (nameToString n))
    return (typ, TACVar t, lines ++ [TACCall (nameToString n) params (Just $ TACVar t)])
tacExpression (Length n@(NameSubscription _ ex)) = do
    let var = nameToString n
    ArraySize size <- gets (unsafeSymbolSize var)
    return (IntType, TACInt (size !! length ex), [])
tacExpression (Length n@(Name _)) = do
    let var = nameToString n
    ArraySize size <- gets (unsafeSymbolSize var)
    return (IntType, TACInt (size !! 0), [])
tacExpression (Var (Name n)) = do
    typ <- gets (unsafeSymbolType n)
    return (typ, TACVar n, [])
tacExpression (Var (NameSubscription n es)) = do
    ex <- tacArraySize es n
    (_, t, lines) <- tacExpression ex
    newvar <- lift popVariable
    typ <- gets (unsafeSymbolType n)
    return (typ, TACVar newvar, lines ++ [TACArrayAccess newvar n t])
tacExpression (Var (NamePointer n)) = do
    newvar <- lift popVariable
    typ <- gets (unsafeSymbolType n)
    return (typ, TACVar newvar, [TACDeRef newvar (TACVar n)])
tacExpression (Address name) = do
    newvar <- lift popVariable
    typ <- gets (unsafeSymbolType (nameToString name))
    case name of
        NameSubscription n es -> tacArraySize es n >>= tacExpression >>= \(_, t, lines) -> return (typ, TACVar newvar, lines ++ [TACAddress newvar (TACArray n t)])
        n -> return (typ, TACVar newvar, [TACAddress newvar (TACVar (nameToString n))])


tacBinaryOperator :: BinaryOperator -> TACBinaryOperator
tacBinaryOperator Plus = TACPlus
tacBinaryOperator Minus = TACMinus
tacBinaryOperator Times = TACTimes
tacBinaryOperator Divide = TACDivide
tacBinaryOperator Equal = TACEqual
tacBinaryOperator Greater = TACGreater
tacBinaryOperator Less = TACLess
tacBinaryOperator NotEqual = TACNotEqual

tacUnaryOperator :: UnaryOperator -> TACUnaryOperator
tacUnaryOperator Not = TACNot
tacUnaryOperator Neg = TACNeg

data TACProgram = TACProgram [TACInstruction] [TACFunction]
    deriving (Eq, Show)

tacData :: TACProgram -> [TACInstruction]
tacData (TACProgram ds _) = ds

tacCode :: TACProgram -> [TACFunction]
tacCode (TACProgram _ is) = is

type TACFunction = [TACInstruction]

data TACInstruction = TACBinary String TACExpression TACBinaryOperator TACExpression
                    | TACUnary String TACUnaryOperator TACExpression
                    | TACCopy String TACExpression
                    | TACArrayDecl String [TACExpression]
                    | TACArrayAccess String String TACExpression
                    | TACArrayModif String TACExpression TACExpression
                    | TACLoad String
                    | TACStore String
                    | TACAddress String TACExpression
                    | TACDeRef String TACExpression
                    | TACDeRefA String TACExpression
                    | TACIf TACExpression String
                    | TACGoto String
                    | TACCall String [TACExpression] (Maybe TACExpression)
                    | TACReturn (Maybe TACExpression)
                    | TACLabel String
                    | TACWrite Type TACExpression
                    | TACRead Type TACExpression
    deriving (Eq, Show, Ord)

data TACBinaryOperator = TACPlus 
                       | TACMinus 
                       | TACTimes 
                       | TACDivide 
                       | TACEqual 
                       | TACGreater 
                       | TACLess 
                       | TACNotEqual
    deriving (Eq, Show, Ord)

data TACUnaryOperator = TACNeg | TACNot
    deriving (Eq, Show, Ord)

data TACExpression = TACInt Int
                   | TACChar Char
                   | TACVar String
                   | TACArray String TACExpression
                   | TACExpr TACExpression TACBinaryOperator TACExpression
    deriving (Eq, Show, Ord)


class TACPrint a where
    tacPrint :: a -> String

tacShowArray :: String -> TACExpression -> String
tacShowArray array ex = array ++ "[" ++ tacPrint ex ++ "]"

instance TACPrint a => TACPrint [a] where
    tacPrint [] = ""
    tacPrint (x:xs) = tacPrint x ++ "\n" ++ tacPrint xs

instance TACPrint TACProgram where
    tacPrint (TACProgram ds is) = tacPrint ds ++ tacPrint (concat is)

instance TACPrint TACInstruction where
    tacPrint (TACBinary var e1 op e2) = var ++ " = " ++ tacPrint e1 ++ tacPrint op ++ tacPrint e2
    tacPrint (TACUnary var op e) = var ++ " = " ++ tacPrint op ++ tacPrint e
    tacPrint (TACCopy var e) = var ++ " = " ++ tacPrint e
    tacPrint (TACArrayDecl s xs) = s ++ " = { " ++ concatMap (\x -> tacPrint x ++ " ") xs ++ " }"
    tacPrint (TACArrayAccess var array ex) = var ++ " = " ++ tacShowArray array ex 
    tacPrint (TACArrayModif array index ex) = tacShowArray array index ++ " = " ++ tacPrint ex
    tacPrint (TACLoad s) = "load " ++ s
    tacPrint (TACStore s) = "store " ++ s
    tacPrint (TACIf e l) = "if " ++ tacPrint e ++ " goto " ++ l
    tacPrint (TACGoto l) = "goto " ++ l
    tacPrint (TACCall l es ret) = intercalate "\n" (map (((++) "param ") . tacPrint) es) ++ "\ncall " ++ l ++ " -> " ++ show ret
    tacPrint (TACReturn Nothing) = "return"
    tacPrint (TACReturn (Just e)) = "return " ++ tacPrint e
    tacPrint (TACLabel l) = l ++ ":"
    tacPrint (TACWrite _ v) = "write " ++ tacPrint v
    tacPrint (TACRead _ e) = "read " ++ tacPrint e
    tacPrint (TACAddress v e) = v ++ " = &" ++ tacPrint e
    tacPrint (TACDeRef v e) = v ++ " = *" ++ tacPrint e
    tacPrint (TACDeRefA v e) = "*" ++ v ++ " = " ++ tacPrint e

instance TACPrint TACBinaryOperator where
    tacPrint TACPlus = "+"
    tacPrint TACMinus = "-"
    tacPrint TACTimes = "*"
    tacPrint TACDivide = "/"
    tacPrint TACEqual = "=="
    tacPrint TACGreater = ">"
    tacPrint TACLess = "<"
    tacPrint TACNotEqual = "!="

instance TACPrint TACUnaryOperator where
    tacPrint TACNeg = "-"
    tacPrint TACNot = "!"

instance TACPrint TACExpression where
    tacPrint (TACInt i) = show i
    tacPrint (TACChar c) = show c
    tacPrint (TACVar s) = s 
    tacPrint (TACArray v e) = v ++ "[" ++ tacPrint e ++ "]"
    tacPrint (TACExpr e1 op e2) = (tacPrint e1) ++ (tacPrint op) ++ (tacPrint e2)