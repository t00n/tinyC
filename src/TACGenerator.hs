module TACGenerator (generateTAC, TACProgram(..), TACInstruction(..), TACBinaryOperator(..), TACUnaryOperator(..), TACExpression(..), TACPrint(..)) where

import Control.Monad.Trans.State (StateT(..), evalStateT)
import Control.Monad.Trans (lift)

import Parser
import MonadNames
import Utility
import SymbolTable

import Data.List(intercalate)

type SSNSS = StateT SymbolTableZipper (Names String String)

infiniteNames :: String -> [String]
infiniteNames s = [s ++ show i | i <- [1..]]

generateTAC :: [Declaration] -> [TACInstruction]
generateTAC xs = do
    evalNames (evalStateT (tacGenerate xs) (zipper emptyST)) (infiniteNames "t") (infiniteNames "l")

class TACGenerator a where
    tacGenerate :: a -> SSNSS [TACInstruction]

instance TACGenerator a => TACGenerator [a] where
    tacGenerate [] = return []
    tacGenerate (x:xs) = do
        first <- tacGenerate x
        rest <- tacGenerate xs
        return $ first ++ rest

instance TACGenerator Declaration where
    tacGenerate (VarDeclaration t (Name n) Nothing) = return $ [TACDeclaration (TACVar n)]
    tacGenerate (VarDeclaration t (Name n) (Just x)) = do
        (t, lines) <- tacExpression x
        return $ lines ++ [TACDeclarationValue (TACVar n) t]
    tacGenerate (VarDeclaration t (NameSubscription n e) Nothing) = do
        (t, lines) <- tacExpression e
        return $ lines ++ [TACDeclaration (TACArray n t)]
    tacGenerate (FuncDeclaration t name params stmt) = do
        functionBody <- tacGenerate stmt
        let newparams = map (\(Parameter t n) -> (nameToString n)) params
        let ret = if functionBody == [] then [TACReturn Nothing] 
            else case last functionBody of
                (TACReturn _) -> []
                _ -> [TACReturn Nothing]
        return $ [TACFunction (nameToString name) newparams] ++ functionBody ++ ret

instance TACGenerator Statement where
    tacGenerate (Assignment (Name n) e) = do
        (t, lines) <- tacExpression e
        return $ lines ++ [TACCopy n t]
    tacGenerate (Assignment (NameSubscription n i) e) = do
        (t1, lines1) <- tacExpression i
        (t2, lines2) <- tacExpression e
        return $ lines1 ++ lines2 ++ [TACArrayModif (TACArray n t1) t2]
    tacGenerate (If e stmt) = do
        (t, lines) <- tacRelExpression e
        labelYes <- lift popLabel
        labelNo <- lift popLabel
        stmt <- tacGenerate stmt
        return $ lines ++ [TACIf t labelYes, TACGoto labelNo, TACLabel labelYes] ++ stmt ++ [TACLabel labelNo]
    tacGenerate (IfElse e s1 s2) = do
        (t, lines) <- tacRelExpression e
        labelYes <- lift popLabel
        labelNo <- lift popLabel
        labelEnd <- lift popLabel
        s1 <- tacGenerate s1
        s2 <- tacGenerate s2
        return $ lines ++ [TACIf t labelYes, TACGoto labelNo, TACLabel labelYes] ++ s1 ++ [TACGoto labelEnd, TACLabel labelNo] ++ s2 ++ [TACLabel labelEnd]
    tacGenerate (While e s) = do
        (t, lines) <- tacRelExpression e
        labelBeg <- lift popLabel
        labelEnd <- lift popLabel
        labelYes <- lift popLabel
        s <- tacGenerate s
        return $ [TACLabel labelBeg] ++ lines ++ [TACIf t labelYes, TACGoto labelEnd, TACLabel labelYes] ++ s ++ [TACGoto labelBeg, TACLabel labelEnd]
    tacGenerate (Return e) = do
        (t, lines) <- tacExpression e
        return $ lines ++ [TACReturn $ Just t]
    tacGenerate (Block ds ss) = do
        ds <- tacGenerate ds
        ss <- tacGenerate ss
        return $ ds ++ ss
    tacGenerate (Write e) = do
        (t, lines) <- tacExpression e
        return $ lines ++ [TACWrite t]
    tacGenerate (Read (Name n)) = return [TACRead (TACVar n)]
    tacGenerate (Read (NameSubscription n e)) = do
        (t, lines) <- tacExpression e
        return $ lines ++ [TACRead (TACArray n t)]
    tacGenerate (Expr e) = do
        (_, lines) <- tacExpression e
        return lines

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
            (t, lines) <- tacExpression ex
            return (TACExpr t TACNotEqual (TACInt 0), lines)
        True -> do
            (t1, lines1) <- tacExpression e1
            (t2, lines2) <- tacExpression e2
            return (TACExpr t1 (tacBinaryOperator op) t2, lines1 ++ lines2)
tacRelExpression (Int i) = return (TACExpr (TACInt i) TACNotEqual (TACInt 0), [])
tacRelExpression (Char c) = return (TACExpr (TACChar c) TACNotEqual (TACInt 0), [])
tacRelExpression (UnOp op e) = 
    case op of
        Neg -> tacRelExpression e
        Not -> do
            (t, lines) <- tacExpression e
            return (TACExpr t TACEqual (TACInt 0), lines)
tacRelExpression (Var v) = do
    (t, lines) <- tacExpression (Var v)
    return (TACExpr t TACNotEqual (TACInt 0), lines)
tacRelExpression e = tacExpression e

tacExpression :: Expression -> SSNSS (TACExpression, [TACInstruction])
tacExpression (Int i) = return (TACInt i, [])
tacExpression (Char i) = return (TACChar i, [])
tacExpression (BinOp e1 op e2) = do
    (t1, lines1) <- tacExpression e1
    (t2, lines2) <- tacExpression e2
    var <- lift popVariable
    let newline = TACBinary var t1 (tacBinaryOperator op) t2
    return (TACVar var, lines1 ++ lines2 ++ [newline])           
tacExpression (UnOp op e) = do
    (t, lines) <- tacExpression e
    var <- lift popVariable
    let newline = TACUnary var (tacUnaryOperator op) t
    return (TACVar var, lines ++ [newline])
tacExpression (Call n es) = do
    reducedES <- mapM tacExpression es
    let params = map fst reducedES
    let lines = concatMap snd reducedES
    t <- lift popVariable
    return (TACVar t, lines ++ [TACCall (nameToString n) (params  ++ [TACVar t])])
tacExpression (Length n) = undefined
tacExpression (Var (Name n)) = return (TACVar n, [])
tacExpression (Var (NameSubscription n e)) = do
    (t, lines) <- tacExpression e
    newvar <- lift popVariable
    return (TACVar newvar, [TACArrayAccess newvar (TACArray n t)] ++ lines)

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

type TACProgram = [TACInstruction]

data TACInstruction = TACDeclaration TACExpression
                    | TACDeclarationValue TACExpression TACExpression
                    | TACFunction String [String]
                    | TACBinary String TACExpression TACBinaryOperator TACExpression
                    | TACUnary String TACUnaryOperator TACExpression
                    | TACCopy String TACExpression
                    | TACArrayAccess String TACExpression
                    | TACArrayModif TACExpression TACExpression
                     -- | TACAddress TACExpression TACExpression
                     -- | TACDeRef TACExpression TACExpression
                     -- | TACDeRefA TACExpression TACExpression
                    | TACIf TACExpression String
                    | TACGoto String
                    | TACCall String [TACExpression]
                    | TACReturn (Maybe TACExpression)
                    | TACLabel String
                    | TACWrite TACExpression
                    | TACRead TACExpression
    deriving (Eq, Show)

data TACBinaryOperator = TACPlus 
                       | TACMinus 
                       | TACTimes 
                       | TACDivide 
                       | TACEqual 
                       | TACGreater 
                       | TACLess 
                       | TACNotEqual
    deriving (Eq, Show)

data TACUnaryOperator = TACNeg | TACNot
    deriving (Eq, Show)

data TACExpression = TACInt Int
                   | TACChar Char
                   | TACVar String
                   | TACArray String TACExpression
                   | TACExpr TACExpression TACBinaryOperator TACExpression
    deriving (Eq, Show)


class TACPrint a where
    tacPrint :: a -> String

instance TACPrint a => TACPrint [a] where
    tacPrint [] = ""
    tacPrint (x:xs) = tacPrint x ++ "\n" ++ tacPrint xs

instance TACPrint TACInstruction where
    tacPrint (TACDeclaration var) = "declare " ++ tacPrint var
    tacPrint (TACFunction f params) = f ++ ":\n" ++ intercalate "\n" (map ((++) "arg ") params)
    tacPrint (TACBinary var e1 op e2) = var ++ " = " ++ tacPrint e1 ++ tacPrint op ++ tacPrint e2
    tacPrint (TACUnary var op e) = var ++ " = " ++ tacPrint op ++ tacPrint e
    tacPrint (TACCopy var e) = var ++ " = " ++ tacPrint e
    tacPrint (TACArrayAccess e1 e2) = e1 ++ " = " ++ tacPrint e2
    tacPrint (TACArrayModif e1 e2) = tacPrint e1 ++ " = " ++ tacPrint e2
    tacPrint (TACIf e l) = "if " ++ tacPrint e ++ " goto " ++ l
    tacPrint (TACGoto l) = "goto " ++ l
    tacPrint (TACCall l es) = intercalate "\n" (map (((++) "param ") . tacPrint) es) ++ "\ncall " ++ l
    tacPrint (TACReturn Nothing) = "return"
    tacPrint (TACReturn (Just e)) = "return " ++ tacPrint e
    tacPrint (TACLabel l) = l ++ ":"
    tacPrint (TACWrite v) = "write " ++ tacPrint v
    tacPrint (TACRead e) = "read " ++ tacPrint e

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
    tacPrint (TACArray s e) = s ++ "[" ++ tacPrint e ++ "]"
    tacPrint (TACExpr e1 op e2) = (tacPrint e1) ++ (tacPrint op) ++ (tacPrint e2)