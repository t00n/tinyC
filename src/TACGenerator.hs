module TACGenerator (tacGenerate, TACProgram(..), TACFunction(..), TACInstruction(..), TACBinaryOperator(..), TACUnaryOperator(..), TACExpression(..), TACPrint(..)) where

import Control.Monad.Trans.State (StateT(..), evalStateT)
import Control.Monad.Trans (lift)
import Control.Monad (liftM2)
import Data.Char (chr, ord)

import Parser
import MonadNames
import Utility
import SymbolTable

import Data.List(intercalate)

type SSNSS = StateT SymbolTable (Names String String)

infiniteNames :: String -> [String]
infiniteNames s = [s ++ show i | i <- [1..]]

tacGenerate :: [Declaration] -> TACProgram
tacGenerate = (,) <$> tacGenerateData <*> tacGenerateText

tacGenerateData :: [Declaration] -> [TACInstruction]
tacGenerateData xs = evalNames (evalStateT (tacGenerateInstructions xs) (zipper emptyST)) (infiniteNames "t") (infiniteNames "l")

tacGenerateText :: [Declaration] -> [[TACInstruction]]
tacGenerateText xs = evalNames (evalStateT (tacGenerateAllFunctions xs) (zipper emptyST)) (infiniteNames "t") (infiniteNames "l")

class TACGenerator a where
    tacGenerateInstructions :: a -> SSNSS [TACInstruction]

instance TACGenerator a => TACGenerator [a] where
    tacGenerateInstructions [] = return []
    tacGenerateInstructions (x:xs) = liftM2 (++) (tacGenerateInstructions x) (tacGenerateInstructions xs)

tacGenerateAllFunctions :: [Declaration] -> SSNSS [[TACInstruction]]
tacGenerateAllFunctions [] = return []
tacGenerateAllFunctions (x:xs) =
    case x of
        FuncDeclaration _ _ _ _ -> liftM2 (:) (tacGenerateFunction x) (tacGenerateAllFunctions xs)
        _ -> tacGenerateAllFunctions xs

tacGenerateFunction :: Declaration -> SSNSS [TACInstruction]
tacGenerateFunction (FuncDeclaration t name params stmt) = do
        functionBody <- tacGenerateInstructions stmt
        let ret = if functionBody == [] then [TACReturn Nothing] 
            else case last functionBody of
                (TACReturn _) -> []
                _ -> [TACReturn Nothing]
        return $ TACLabel (nameToString name) : functionBody ++ ret

instance TACGenerator Declaration where
    tacGenerateInstructions (VarDeclaration IntType (Name n) Nothing) = return $ [TACCopy n (TACInt 0)]
    tacGenerateInstructions (VarDeclaration CharType (Name n) Nothing) = return $ [TACCopy n (TACChar (chr 0))]
    tacGenerateInstructions (VarDeclaration IntType (Name n) (Just (Int x))) = return $ [TACCopy n (TACInt x)]
    tacGenerateInstructions (VarDeclaration CharType (Name n) (Just (Char c))) = return $ [TACCopy n (TACChar c)]
    tacGenerateInstructions (VarDeclaration _ (Name n) (Just e)) = do
        (t, lines) <- tacExpression e
        return $ lines ++ [TACCopy n t]
    tacGenerateInstructions (VarDeclaration IntType (NameSubscription n e) _) = 
        let size (Int i) = i
            size (Char c) = ord c
        in return $ [TACArrayDecl n (replicate (size e) (TACInt 0))]
    tacGenerateInstructions (VarDeclaration CharType (NameSubscription n e) _) = 
        let size (Int i) = i
            size (Char c) = ord c
        in return $ [TACArrayDecl n (replicate (size e) (TACChar (chr 0)))]
    tacGenerateInstructions _ = return []


instance TACGenerator Statement where
    tacGenerateInstructions (Assignment (Name n) e) = do
        (t, lines) <- tacExpression e
        return $ lines ++ [TACCopy n t]
    tacGenerateInstructions (Assignment (NameSubscription n i) e) = do
        (t1, lines1) <- tacExpression i
        (t2, lines2) <- tacExpression e
        return $ lines1 ++ lines2 ++ [TACArrayModif (TACArray n t1) t2]
    tacGenerateInstructions (If e stmt) = do
        (t, lines) <- tacRelExpression e
        labelYes <- lift popLabel
        labelNo <- lift popLabel
        stmt <- tacGenerateInstructions stmt
        return $ lines ++ [TACIf t labelYes, TACGoto labelNo, TACLabel labelYes] ++ stmt ++ [TACLabel labelNo]
    tacGenerateInstructions (IfElse e s1 s2) = do
        (t, lines) <- tacRelExpression e
        labelYes <- lift popLabel
        labelNo <- lift popLabel
        labelEnd <- lift popLabel
        s1 <- tacGenerateInstructions s1
        s2 <- tacGenerateInstructions s2
        return $ lines ++ [TACIf t labelYes, TACGoto labelNo, TACLabel labelYes] ++ s1 ++ [TACGoto labelEnd, TACLabel labelNo] ++ s2 ++ [TACLabel labelEnd]
    tacGenerateInstructions (While e s) = do
        (t, lines) <- tacRelExpression e
        labelBeg <- lift popLabel
        labelEnd <- lift popLabel
        labelYes <- lift popLabel
        s <- tacGenerateInstructions s
        return $ [TACLabel labelBeg] ++ lines ++ [TACIf t labelYes, TACGoto labelEnd, TACLabel labelYes] ++ s ++ [TACGoto labelBeg, TACLabel labelEnd]
    tacGenerateInstructions (Return e) = do
        (t, lines) <- tacExpression e
        return $ lines ++ [TACReturn $ Just t]
    tacGenerateInstructions (Block ds ss) = do
        ds <- tacGenerateInstructions ds
        ss <- tacGenerateInstructions ss
        return $ ds ++ ss
    tacGenerateInstructions (Write e) = do
        (t, lines) <- tacExpression e
        return $ lines ++ [TACWrite t]
    tacGenerateInstructions (Read (Name n)) = return [TACRead (TACVar n)]
    tacGenerateInstructions (Read n) = do
        (t, lines) <- tacExpression (Var n)
        return $ lines ++ [TACRead t]
    tacGenerateInstructions (Expr e) = do
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
    return (TACVar t, lines ++ [TACCall (nameToString n) params (Just $ TACVar t)])
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

type TACProgram = ([TACInstruction], [TACFunction])

type TACFunction = [TACInstruction]

data TACInstruction = TACBinary String TACExpression TACBinaryOperator TACExpression
                    | TACUnary String TACUnaryOperator TACExpression
                    | TACCopy String TACExpression
                    | TACArrayDecl String [TACExpression]
                    | TACArrayAccess String TACExpression
                    | TACArrayModif TACExpression TACExpression
                    | TACLoad String
                    | TACStore String
                     -- | TACAddress TACExpression TACExpression
                     -- | TACDeRef TACExpression TACExpression
                     -- | TACDeRefA TACExpression TACExpression
                    | TACIf TACExpression String
                    | TACGoto String
                    | TACCall String [TACExpression] (Maybe TACExpression)
                    | TACReturn (Maybe TACExpression)
                    | TACLabel String
                    | TACWrite TACExpression
                    | TACRead TACExpression
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
                   | TACVar String
                   | TACArray String TACExpression
                   | TACExpr TACExpression TACBinaryOperator TACExpression
    deriving (Eq, Show, Ord)


class TACPrint a where
    tacPrint :: a -> String

instance TACPrint a => TACPrint [a] where
    tacPrint [] = ""
    tacPrint (x:xs) = tacPrint x ++ "\n" ++ tacPrint xs

instance TACPrint TACInstruction where
    tacPrint (TACBinary var e1 op e2) = var ++ " = " ++ tacPrint e1 ++ tacPrint op ++ tacPrint e2
    tacPrint (TACUnary var op e) = var ++ " = " ++ tacPrint op ++ tacPrint e
    tacPrint (TACCopy var e) = var ++ " = " ++ tacPrint e
    tacPrint (TACArrayDecl s xs) = s ++ " = { " ++ concatMap (\x -> tacPrint x ++ " ") xs ++ " }"
    tacPrint (TACArrayAccess e1 e2) = e1 ++ " = " ++ tacPrint e2
    tacPrint (TACArrayModif e1 e2) = tacPrint e1 ++ " = " ++ tacPrint e2
    tacPrint (TACLoad s) = "load " ++ s
    tacPrint (TACStore s) = "store " ++ s
    tacPrint (TACIf e l) = "if " ++ tacPrint e ++ " goto " ++ l
    tacPrint (TACGoto l) = "goto " ++ l
    tacPrint (TACCall l es _) = intercalate "\n" (map (((++) "param ") . tacPrint) es) ++ "\ncall " ++ l
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