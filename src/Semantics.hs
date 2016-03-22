module Semantics (checkSemantics, SemanticError(..), ErrorType(..)) where

import Control.Monad (void, foldM)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, fromJust)

import Parser

data Kind = V | F | A
    deriving (Eq, Show)

data Info = Info {
    infoType :: Type,
    infoKind :: Kind
} deriving (Eq, Show)

type Symbols = Map.Map String Info

data SymbolTable = SymbolTable {
    symbols :: Symbols,
    parent :: Maybe SymbolTable
} deriving (Eq, Show)

data ErrorType = NotDeclaredError | NotAFunctionError
    deriving (Eq, Show)

data SemanticError = SemanticError {
    errorType :: ErrorType,
    errorVariable :: String
} deriving (Eq, Show)

emptySymbolTable :: Maybe SymbolTable -> SymbolTable
emptySymbolTable = SymbolTable Map.empty

insertSymbol :: String -> Info -> SymbolTable -> SymbolTable
insertSymbol s i = SymbolTable <$> Map.insert s i . symbols <*> parent

variableName :: Variable -> String
variableName (Variable s) = s
variableName (Array s _) = s
variableName _ = error "Not a variable declaration"

variableArray :: Variable -> Bool
variableArray (Array _ _) = True
variableArray (Variable _) = False
variableArray _ = error "Not a variable declaration"

getNameInfo :: String -> SymbolTable -> Maybe Info
getNameInfo s st = let res = Map.lookup s (symbols st) in 
                       if res == Nothing then parent st >>= getNameInfo s
                       else res

variableInScope :: String -> SymbolTable -> Bool
variableInScope n st = Map.member n (symbols st) || variableInParent n st
    where variableInParent _ (SymbolTable _ Nothing) = False
          variableInParent n (SymbolTable s (Just p)) = variableInScope n p

walkProgram :: Program -> SymbolTable -> Either SemanticError SymbolTable
walkProgram [] symbolTable = Right symbolTable
walkProgram (x:xs) symbolTable =
    case x of
        VarDeclaration t e _ -> walkProgram xs 
                                    (insertSymbol (variableName e) (Info t (if variableArray e then A else V)) symbolTable)
        FuncDeclaration t e params stmt -> walkProgram xs
                                    (insertSymbol (variableName e) (Info t F) symbolTable) >>= checkStatement stmt 

checkStatement :: Statement -> SymbolTable -> Either SemanticError SymbolTable
checkStatement stmt st = 
    case stmt of
        Assignment v e -> checkVariable v st >> checkExpression e st
        If e stmt1 -> checkExpression e st >> checkStatement stmt1 st
        IfElse e stmt1 stmt2 -> checkExpression e st >> checkStatement stmt1 st >> checkStatement stmt2 st
        While e stmt1 -> checkExpression e st >> checkStatement stmt1 st
        Return e -> checkExpression e st
        Block decl stmts -> walkProgram decl (emptySymbolTable $ Just st) >> checkStatements stmts st
        Write e -> checkExpression e st
        Read v -> checkVariable v st
        Expr e -> checkExpression e st

checkStatements :: [Statement] -> SymbolTable -> Either SemanticError SymbolTable
checkStatements [] st = Right st
checkStatements (x:xs) st = checkStatement x st >> checkStatements xs st

checkExpression :: Expression -> SymbolTable -> Either SemanticError SymbolTable
checkExpression expr st = 
    case expr of 
        BinOp e1 _ e2 -> checkExpression e1 st >> checkExpression e2 st
        UnOp _ e -> checkExpression e st
        Call v@(Variable s) params -> checkVariable v st >> if infoKind (fromJust (getNameInfo s st)) /= F then Left (SemanticError NotAFunctionError s) else Right st >> foldM (flip checkExpression) st params
        Call (Array s _) _ -> Left (SemanticError NotAFunctionError s)
        Var v -> checkVariable v st
        _ -> Right st

checkVariable :: Variable -> SymbolTable -> Either SemanticError SymbolTable
checkVariable var st = 
    let inScope s = if variableInScope s st then Right st else Left (SemanticError NotDeclaredError s) in
    case var of
        Variable s -> inScope s
        Array s e -> inScope s >> checkExpression e st

checkSemantics :: Program -> Either SemanticError ()
checkSemantics = void . flip walkProgram (emptySymbolTable Nothing)
