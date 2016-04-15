module TACGenerator (generateTAC, TACProgram(..), TACLine(..), TACInstruction(..), TACExpression(..), TACVariable(..)) where

import Parser
import MonadNames
import Utility

infiniteNames :: String -> [String]
infiniteNames s = [s ++ show i | i <- [1..]]

generateTAC :: [Declaration] -> TACProgram
generateTAC xs = do
    evalNames (tacGenerate xs) (infiniteNames "t") (infiniteNames "l")

class TACGenerator a where
    tacGenerate :: a -> Names String String TACProgram

instance TACGenerator a => TACGenerator [a] where
    tacGenerate [] = return $ TACProgram []
    tacGenerate (x:xs) = do
        (TACProgram rest) <- tacGenerate xs
        (TACProgram first) <- tacGenerate x
        return $ TACProgram $ first ++ rest

instance TACGenerator Declaration where
    tacGenerate (VarDeclaration t name Nothing) = return $ TACProgram [TACLine Nothing $ TACCopy (TACVar $ nameString name) (TACInt 0)]
    tacGenerate (FuncDeclaration t name params stmt) = undefined

instance TACGenerator Statement where
    tacGenerate stmt =  undefined

instance TACGenerator Expression where
    tacGenerate expr = undefined

newtype TACProgram = TACProgram [TACLine]
    deriving (Eq)

instance Show TACProgram where
    show (TACProgram []) = ""
    show (TACProgram (x:xs)) = show x ++ "\n" ++ show (TACProgram xs)

data TACLine = TACLine (Maybe TACLabel) TACInstruction
    deriving (Eq)

instance Show TACLine where
    show (TACLine Nothing i) = "\t " ++ show i
    show (TACLine (Just x) i) = x ++ " " ++ show i 

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
    deriving (Eq)

instance Show TACInstruction where
    show (TACCopy var expr) = show var ++ " = " ++ show expr

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
    deriving (Eq)

instance Show TACExpression where
    show (TACInt i) = show i
    show (TACChar c) = show c
    show (TACExpr var) = show var

data TACVariable = TACVar String
                 | TACArray String
    deriving (Eq)

instance Show TACVariable where
    show (TACVar str) = str
    show (TACArray str) = str