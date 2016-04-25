module TACGenerator (generateTAC, TACProgram(..), TACLine(..), TACInstruction(..), TACBinaryOperator(..), TACUnaryOperator(..), TACRelationOperator(..), TACExpression(..), PrettyPrintable(..)) where

import Parser
import MonadNames
import Utility

infiniteNames :: String -> [String]
infiniteNames s = [s ++ show i | i <- [1..]]

generateTAC :: [Declaration] -> [TACLine]
generateTAC xs = do
    evalNames (tacGenerate xs) (infiniteNames "t") (infiniteNames "l")

class TACGenerator a where
    tacGenerate :: a -> Names String String [TACLine]

instance TACGenerator a => TACGenerator [a] where
    tacGenerate [] = return []
    tacGenerate (x:xs) = do
        first <- tacGenerate x
        rest <- tacGenerate xs
        return $ first ++ rest

instance TACGenerator Declaration where
    tacGenerate (VarDeclaration t name Nothing) = return $ [TACLine Nothing $ TACCopy (nameString name) (TACInt 0)]
    tacGenerate (VarDeclaration t name (Just x)) = do
        (t, lines) <- tacExpression x
        return $ lines ++ [TACLine Nothing $ TACCopy (nameString name) t]
    tacGenerate (FuncDeclaration t name params stmt) = undefined

instance TACGenerator Statement where
    tacGenerate stmt = undefined

instance TACGenerator Expression where
    tacGenerate expr = undefined

tacExpression :: Expression -> Names String String (TACExpression, [TACLine])
tacExpression (Int i) = return (TACInt i, [])
tacExpression (Char i) = return (TACChar i, [])
tacExpression (BinOp e1 op e2) = do
    (t1, lines1) <- tacExpression e1
    (t2, lines2) <- tacExpression e2
    var <- popVariable
    let newline = TACLine Nothing (TACBinary var t1 (tacBinaryOperator op) t2)
    return (TACVar var, lines1 ++ lines2 ++ [newline])           
tacExpression (UnOp op e) = do
    (t, lines) <- tacExpression e
    var <- popVariable
    let newline = TACLine Nothing (TACUnary var (tacUnaryOperator op) t)
    return (TACVar var, lines ++ [newline])
tacExpression (Call n es) = undefined
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

type TACProgram = [TACLine]

data TACLine = TACLine (Maybe TACLabel) TACInstruction
    deriving (Eq, Show)

type TACLabel = String

data TACInstruction = TACBinary String TACExpression TACBinaryOperator TACExpression
                     | TACUnary String TACUnaryOperator TACExpression
                     | TACCopy String TACExpression
                     | TACIf TACExpression TACRelationOperator TACExpression String
                     | TACGoto String
                     | TACCall String
                     | TACArrayAccess String String TACExpression
                     | TACArrayModif String TACExpression String
                     -- | TACAddress TACExpression TACExpression
                     -- | TACDeRef TACExpression TACExpression
                     -- | TACDeRefA TACExpression TACExpression
                     | TACReturn TACExpression
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

instance PrettyPrintable TACLine where
    prettyPrint (TACLine Nothing i) = "\t" ++ prettyPrint i
    prettyPrint (TACLine (Just l) i) = l ++ prettyPrint i

instance PrettyPrintable TACInstruction where
    prettyPrint (TACBinary var e1 op e2) = var ++ " = " ++ prettyPrint e1 ++ prettyPrint op ++ prettyPrint e2
    prettyPrint (TACUnary var op e) = var ++ " = " ++ prettyPrint op ++ prettyPrint e
    prettyPrint (TACCopy var e) = var ++ " = " ++ prettyPrint e
    prettyPrint (TACIf e1 op e2 l) = "if " ++ prettyPrint e1 ++ prettyPrint op ++ prettyPrint e2 ++ " goto " ++ l
    prettyPrint (TACGoto l) = "goto " ++ l
    prettyPrint (TACCall l) = "call " ++ l
    prettyPrint (TACArrayAccess v1 v2 e) = v1 ++ " = " ++ "v2[" ++ prettyPrint e ++ "]"
    prettyPrint (TACArrayModif v1 e v2) = v1 ++ "[" ++ prettyPrint e ++ "] = " ++ v2
    prettyPrint (TACReturn e) = "return " ++ prettyPrint e

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