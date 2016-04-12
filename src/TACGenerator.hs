module TACGenerator (tacGenerate) where

import Parser
import Utility

class TACGenerator a where
    tacGenerate :: a -> [TACLine]

instance TACGenerator a => TACGenerator [a] where
    tacGenerate = (reverse ... concatMap) tacGenerate

instance TACGenerator Declaration where
    tacGenerate (VarDeclaration t name expr) = undefined
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