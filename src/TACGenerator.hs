module TACGenerator (generateTAC, TACProgram(..), TACInstruction(..), TACBinaryOperator(..), TACUnaryOperator(..), TACExpression(..), PrettyPrintable(..)) where

import Parser
import MonadNames
import Utility

import Data.List(intercalate)

infiniteNames :: String -> [String]
infiniteNames s = [s ++ show i | i <- [1..]]

generateTAC :: [Declaration] -> [TACInstruction]
generateTAC xs = do
    evalNames (tacGenerate xs) (infiniteNames "t") (infiniteNames "l")

class TACGenerator a where
    tacGenerate :: a -> Names String String [TACInstruction]

instance TACGenerator a => TACGenerator [a] where
    tacGenerate [] = return []
    tacGenerate (x:xs) = do
        first <- tacGenerate x
        rest <- tacGenerate xs
        return $ first ++ rest

instance TACGenerator Declaration where
    tacGenerate (VarDeclaration t name Nothing) = return $ [TACDeclaration (nameString name)]
    tacGenerate (VarDeclaration t name (Just x)) = do
        (t, lines) <- tacExpression x
        return $ [TACDeclaration (nameString name)] ++ lines ++ [TACCopy (nameString name) t]
    tacGenerate (FuncDeclaration t name params stmt) = do
        functionBody <- tacGenerate stmt
        params <- tacGenerate params
        return $ [TACLabel (nameString name)] ++ params ++ functionBody ++ [TACReturn (TACInt 0)]

instance TACGenerator Parameter where
    tacGenerate (Parameter t n) = return [TACParam (nameString n)]

instance TACGenerator Statement where
    tacGenerate (Assignment n e) = do
        (t, lines) <- tacExpression e
        return $ lines ++ [TACCopy (nameString n) t]
    tacGenerate (If e stmt) = do
        (t, lines) <- tacExpression e
        labelYes <- popLabel
        labelNo <- popLabel
        stmt <- tacGenerate stmt
        return $ lines ++ [TACIf t labelYes, TACGoto labelNo, TACLabel labelYes] ++ stmt ++ [TACLabel labelNo]
    tacGenerate (IfElse e s1 s2) = do
        (t, lines) <- tacExpression e
        labelYes <- popLabel
        labelNo <- popLabel
        labelEnd <- popLabel
        s1 <- tacGenerate s1
        s2 <- tacGenerate s2
        return $ lines ++ [TACIf t labelYes, TACGoto labelNo, TACLabel labelYes] ++ s1 ++ [TACGoto labelEnd, TACLabel labelNo] ++ s2 ++ [TACLabel labelEnd]
    tacGenerate (While e s) = do
        (t, lines) <- tacExpression e
        labelBeg <- popLabel
        labelEnd <- popLabel
        labelYes <- popLabel
        s <- tacGenerate s
        return $ [TACLabel labelBeg] ++ lines ++ [TACIf t labelYes, TACGoto labelEnd, TACLabel labelYes] ++ s ++ [TACGoto labelBeg, TACLabel labelEnd]
    tacGenerate (Return e) = do
        (t, lines) <- tacExpression e
        return $ lines ++ [TACReturn t]
    tacGenerate (Block ds ss) = do
        ds <- tacGenerate ds
        ss <- tacGenerate ss
        return $ ds ++ ss
    tacGenerate (Write e) = do
        (t, lines) <- tacExpression e
        return $ lines ++ [TACWrite t]
    tacGenerate (Expr e) = do
        (_, lines) <- tacExpression e
        return lines

tacExpression :: Expression -> Names String String (TACExpression, [TACInstruction])
tacExpression (Int i) = return (TACInt i, [])
tacExpression (Char i) = return (TACChar i, [])
tacExpression (BinOp e1 op e2) = do
    (t1, lines1) <- tacExpression e1
    (t2, lines2) <- tacExpression e2
    var <- popVariable
    let newline = TACBinary var t1 (tacBinaryOperator op) t2
    return (TACVar var, lines1 ++ lines2 ++ [newline])           
tacExpression (UnOp op e) = do
    (t, lines) <- tacExpression e
    var <- popVariable
    let newline = TACUnary var (tacUnaryOperator op) t
    return (TACVar var, lines ++ [newline])
tacExpression (Call n es) = do
    reducedES <- mapM tacExpression es
    let params = map fst reducedES
    let lines = concatMap snd reducedES
    t <- popVariable
    return (TACVar t, lines ++ [TACCall (nameString n) (params  ++ [TACVar t])])
tacExpression (Length n) = undefined
tacExpression (Var n) = return (TACVar (nameString n), [])

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

data TACInstruction = TACDeclaration String
                    | TACParam String
                    | TACBinary String TACExpression TACBinaryOperator TACExpression
                    | TACUnary String TACUnaryOperator TACExpression
                    | TACCopy String TACExpression
                    | TACIf TACExpression String
                    | TACGoto String
                    | TACCall String [TACExpression]
                    | TACArrayAccess String String TACExpression
                    | TACArrayModif String TACExpression String
                     -- | TACAddress TACExpression TACExpression
                     -- | TACDeRef TACExpression TACExpression
                     -- | TACDeRefA TACExpression TACExpression
                    | TACReturn TACExpression
                    | TACLabel String
                    | TACWrite TACExpression
                    | TACRead String
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
    deriving (Eq, Show)


class PrettyPrintable a where
    prettyPrint :: a -> String

instance PrettyPrintable a => PrettyPrintable [a] where
    prettyPrint [] = ""
    prettyPrint (x:xs) = prettyPrint x ++ "\n" ++ prettyPrint xs

instance PrettyPrintable TACInstruction where
    prettyPrint (TACDeclaration var) = "declare " ++ var
    prettyPrint (TACParam var) = "arg " ++ var
    prettyPrint (TACBinary var e1 op e2) = var ++ " = " ++ prettyPrint e1 ++ prettyPrint op ++ prettyPrint e2
    prettyPrint (TACUnary var op e) = var ++ " = " ++ prettyPrint op ++ prettyPrint e
    prettyPrint (TACCopy var e) = var ++ " = " ++ prettyPrint e
    prettyPrint (TACIf e l) = "if " ++ prettyPrint e ++ " goto " ++ l
    prettyPrint (TACGoto l) = "goto " ++ l
    prettyPrint (TACCall l es) = intercalate "\n" (map (((++) "param ") . prettyPrint) es) ++ "call " ++ l
    prettyPrint (TACArrayAccess v1 v2 e) = v1 ++ " = " ++ "v2[" ++ prettyPrint e ++ "]"
    prettyPrint (TACArrayModif v1 e v2) = v1 ++ "[" ++ prettyPrint e ++ "] = " ++ v2
    prettyPrint (TACReturn e) = "return " ++ prettyPrint e
    prettyPrint (TACLabel l) = l ++ ":"
    prettyPrint (TACWrite v) = "write " ++ prettyPrint v

instance PrettyPrintable TACBinaryOperator where
    prettyPrint TACPlus = "+"
    prettyPrint TACMinus = "-"
    prettyPrint TACTimes = "*"
    prettyPrint TACDivide = "/"
    prettyPrint TACEqual = "=="
    prettyPrint TACGreater = ">"
    prettyPrint TACLess = "<"
    prettyPrint TACNotEqual = "!="

instance PrettyPrintable TACUnaryOperator where
    prettyPrint TACNeg = "-"
    prettyPrint TACNot = "!"

instance PrettyPrintable TACExpression where
    prettyPrint (TACInt i) = show i
    prettyPrint (TACChar c) = show c
    prettyPrint (TACVar s) = s 