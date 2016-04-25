module TACGenerator (generateTAC, showTACProgram, TACProgram(..), TACLine(..), TACInstruction(..), TACExpression(..)) where

import Parser
import MonadNames
import Utility

infiniteNames :: String -> [String]
infiniteNames s = [s ++ show i | i <- [1..]]

generateTAC :: [Declaration] -> [TACLine]
generateTAC xs = do
    evalNames (tacGenerate xs) (infiniteNames "t") (infiniteNames "l")

showTACProgram :: TACProgram -> String
showTACProgram [] = ""
showTACProgram (x:xs) = (show x) ++ "\n" ++ (showTACProgram xs)

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


                                 
tacExpression (UnOp op e) = undefined
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