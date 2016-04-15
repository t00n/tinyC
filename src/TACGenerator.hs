module TACGenerator (generateTAC) where

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
    tacGenerate (x:xs) = tacGenerate xs >> tacGenerate x

instance TACGenerator Declaration where
    tacGenerate (VarDeclaration t name expr) = undefined
    tacGenerate (FuncDeclaration t name params stmt) = undefined

instance TACGenerator Statement where
    tacGenerate stmt =  undefined

instance TACGenerator Expression where
    tacGenerate expr = undefined

type TACLine = (Maybe TACLabel, TACInstruction)

type TACLabel = String

data TACInstruction = TACBinary TACVariable TACExpression TACBinaryOperator TACExpression
                     | TACUnary TACVariable TACUnaryOperator TACExpression
                     | TACCopy TACVariable TACExpression
                     | TACIf TACExpression TACRelationOperator TACExpression String
                     | TACGoto String
                     | TACCall String
                     | TACArrayAccess TACVariable TACVariable TACExpression
                     | TACArrayModif TACVariable TACExpression TACVariable
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
                   | TACExpr TACVariable
    deriving (Eq, Show)

data TACVariable = TACVar String
                 | TACArray String
    deriving (Eq, Show)