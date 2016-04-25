module TACGenerator (generateTAC, TACProgram(..), TACInstruction(..), TACBinaryOperator(..), TACUnaryOperator(..), TACRelationOperator(..), TACExpression(..), PrettyPrintable(..)) where

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
    tacGenerate (Block ds ss) = do
        ds <- tacGenerate ds
        ss <- tacGenerate ss
        return $ ds ++ ss
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

tacRelationOperator :: BinaryOperator -> TACRelationOperator
tacRelationOperator Equal = TACEqual
tacRelationOperator Greater = TACGreater
tacRelationOperator Less = TACLess
tacRelationOperator NotEqual = TACNotEqual

tacUnaryOperator :: UnaryOperator -> TACUnaryOperator
tacUnaryOperator Not = TACNot
tacUnaryOperator Neg = TACNeg

type TACProgram = [TACInstruction]

data TACInstruction = TACDeclaration String
                    | TACParam String
                    | TACBinary String TACExpression TACBinaryOperator TACExpression
                    | TACUnary String TACUnaryOperator TACExpression
                    | TACCopy String TACExpression
                    | TACIf TACExpression TACRelationOperator TACExpression String
                    | TACGoto String
                    | TACCall String [TACExpression]
                    | TACArrayAccess String String TACExpression
                    | TACArrayModif String TACExpression String
                     -- | TACAddress TACExpression TACExpression
                     -- | TACDeRef TACExpression TACExpression
                     -- | TACDeRefA TACExpression TACExpression
                    | TACReturn TACExpression
                    | TACLabel String
    deriving (Eq, Show)

data TACBinaryOperator = TACPlus 
                       | TACMinus 
                       | TACTimes 
                       | TACDivide 
    deriving (Eq, Show)

data TACUnaryOperator = TACNeg | TACNot
    deriving (Eq, Show)

data TACRelationOperator = TACEqual 
                         | TACGreater 
                         | TACLess 
                         | TACNotEqual
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
    prettyPrint (TACBinary var e1 op e2) = var ++ " = " ++ prettyPrint e1 ++ prettyPrint op ++ prettyPrint e2
    prettyPrint (TACUnary var op e) = var ++ " = " ++ prettyPrint op ++ prettyPrint e
    prettyPrint (TACCopy var e) = var ++ " = " ++ prettyPrint e
    prettyPrint (TACIf e1 op e2 l) = "if " ++ prettyPrint e1 ++ prettyPrint op ++ prettyPrint e2 ++ " goto " ++ l
    prettyPrint (TACGoto l) = "goto " ++ l
    prettyPrint (TACCall l es) = intercalate "\n" (map (((++) "param ") . prettyPrint) es) ++ "call " ++ l
    prettyPrint (TACArrayAccess v1 v2 e) = v1 ++ " = " ++ "v2[" ++ prettyPrint e ++ "]"
    prettyPrint (TACArrayModif v1 e v2) = v1 ++ "[" ++ prettyPrint e ++ "] = " ++ v2
    prettyPrint (TACReturn e) = "return " ++ prettyPrint e
    prettyPrint (TACLabel l) = l ++ ":"

instance PrettyPrintable TACBinaryOperator where
    prettyPrint TACPlus = "+"
    prettyPrint TACMinus = "-"
    prettyPrint TACTimes = "*"
    prettyPrint TACDivide = "/"

instance PrettyPrintable TACUnaryOperator where
    prettyPrint TACNeg = "-"
    prettyPrint TACNot = "!"

instance PrettyPrintable TACRelationOperator where
    prettyPrint TACEqual = "=="
    prettyPrint TACGreater = ">"
    prettyPrint TACLess = "<"
    prettyPrint TACNotEqual = "!="

instance PrettyPrintable TACExpression where
    prettyPrint (TACInt i) = show i
    prettyPrint (TACChar c) = show c
    prettyPrint (TACVar s) = s 