module TACProgram where

import Data.List(intercalate)

import AST

data TACProgram = TACProgram [TACInstruction] [TACFunction]
    deriving (Eq, Show)

tacData :: TACProgram -> [TACInstruction]
tacData (TACProgram ds _) = ds

tacCode :: TACProgram -> [TACFunction]
tacCode (TACProgram _ is) = is

type TACFunction = [TACInstruction]

data TACInstruction = TACBinary Variable TACExpression TACBinaryOperator TACExpression
                    | TACUnary Variable TACUnaryOperator TACExpression
                    | TACCopy Variable TACExpression
                    | TACArrayDecl Variable [TACExpression]
                    | TACArrayAccess Variable Variable TACExpression
                    | TACArrayModif Variable TACExpression TACExpression
                    | TACLoad Variable
                    | TACStore Variable
                    | TACAddress Variable TACExpression
                    | TACDeRef Variable TACExpression
                    | TACDeRefA Variable TACExpression
                    | TACIf TACExpression Label
                    | TACGoto Label
                    | TACCall Label [TACExpression] (Maybe TACExpression)
                    | TACReturn (Maybe TACExpression)
                    | TACLabel Label
                    | TACWrite TACProgram.Type TACExpression
                    | TACRead TACProgram.Type TACExpression
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
                   | TACVar Variable
                   | TACArray Variable TACExpression
                   | TACExpr TACExpression TACBinaryOperator TACExpression
                   | TACUnOp TACUnaryOperator TACExpression
    deriving (Eq, Show, Ord)

type Type = AST.Type

type Label = String
type Variable = String

class TACPrint a where
    tacPrint :: a -> String

tacShowArray :: Variable -> TACExpression -> String
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