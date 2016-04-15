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
    tacGenerate (VarDeclaration t name expr) = do
        newName <- nextVariable
        return [(0, TACReturn $ TACVar newName)]
    tacGenerate (FuncDeclaration t name params stmt) = undefined

instance TACGenerator Statement where
    tacGenerate stmt =  undefined

instance TACGenerator Expression where
    tacGenerate expr = undefined

type TACLine = (Int, TACInstruction)

data TACInstruction = TACAssignment2 TACExpr TACExpr TACBinaryOperator TACExpr
                     | TACAssignment1 TACExpr TACUnaryOperator TACExpr
                     | TACAssignment0 TACExpr TACExpr
                     | TACIf TACExpr TACRelationOperator TACExpr TACInstruction
                     | TACGoto Int
                     | TACCall Int
                     | TACArrayAccess TACExpr TACExpr TACExpr
                     | TACArrayModif TACExpr TACExpr TACExpr
                     -- | TACAddress TACExpr TACExpr
                     -- | TACDeRef TACExpr TACExpr
                     -- | TACDeRefA TACExpr TACExpr
                     | TACReturn TACExpr
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

data TACExpr = TACInt Int
             | TACChar Char
             | TACVar String
             | TACArray String
    deriving (Eq, Show)