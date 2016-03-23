module Semantics (checkSemantics, SemanticError(..), ErrorType(..)) where

import Control.Monad (void, foldM)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, fromJust)

import Parser

data Kind = VariableKind | FunctionKind | ArrayKind
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

data ErrorType = NotDeclaredError | NotAFunctionError | NameExistsError | NotAnArrayError
    deriving (Eq, Show)

data SemanticError = SemanticError {
    errorType :: ErrorType,
    errorVariable :: String
} deriving (Eq, Show)

emptySymbolTable :: Maybe SymbolTable -> SymbolTable
emptySymbolTable = SymbolTable Map.empty

insertSymbol :: String -> Info -> SymbolTable -> SymbolTable
insertSymbol s i = SymbolTable <$> Map.insert s i . symbols <*> parent

getSymbolInfo :: String -> SymbolTable -> Maybe Info
getSymbolInfo s st = let res = Map.lookup s (symbols st) in 
                       if res == Nothing then parent st >>= getSymbolInfo s
                       else res

symbolIsInScope :: String -> SymbolTable -> Bool
symbolIsInScope n st = Map.member n (symbols st) || variableInParent n st
    where variableInParent _ (SymbolTable _ Nothing) = False
          variableInParent n (SymbolTable s (Just p)) = symbolIsInScope n p

symbolExists :: String -> SymbolTable -> Bool
symbolExists n st = Map.member n (symbols st)

symbolIsKind :: String -> Kind -> SymbolTable -> Bool
symbolIsKind s k st = let info = getSymbolInfo s st in
    if isJust info then infoKind (fromJust info) == k
    else False

symbolIsType :: String -> Type -> SymbolTable -> Bool
symbolIsType s t st = let info = getSymbolInfo s st in
    if isJust info then infoType (fromJust info) == t
    else False

nameString :: Name -> String
nameString (Name s) = s
nameString (NameSubscription s _) = s

nameKind :: Name -> Kind
nameKind (NameSubscription _ _) = ArrayKind
nameKind (Name _) = VariableKind

walkProgram :: Program -> SymbolTable -> Either SemanticError SymbolTable
walkProgram [] symbolTable = Right symbolTable
walkProgram (x:xs) symbolTable =
    let newSymbolTable t e k = if symbolExists (nameString e) symbolTable then Left (SemanticError NameExistsError (nameString e))
                               else Right $ insertSymbol (nameString e) (Info t k) symbolTable 
        in
        case x of
            VarDeclaration t e _ -> newSymbolTable t e (nameKind e) >>= walkProgram xs  
            FuncDeclaration t e params stmt -> newSymbolTable t e FunctionKind >>= walkProgram xs >>= checkStatement stmt 

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
checkStatements = flip $ foldM $ flip checkStatement

checkExpression :: Expression -> SymbolTable -> Either SemanticError SymbolTable
checkExpression expr st = 
    case expr of 
        BinOp e1 _ e2 -> checkExpression e1 st >> checkExpression e2 st
        UnOp _ e -> checkExpression e st
        Call v@(Name s) params -> 
            checkVariable v st >> 
            if symbolIsKind s FunctionKind st then Right st
            else Left (SemanticError NotAFunctionError s) 
            >> foldM (flip checkExpression) st params
        Call (NameSubscription s _) _ -> Left (SemanticError NotAFunctionError s)
        Length v@(Name s) -> checkVariable v st >> 
            if symbolIsKind s ArrayKind st then Right st
            else Left (SemanticError NotAnArrayError s)
        Var v -> checkVariable v st
        _ -> Right st

checkVariable :: Name -> SymbolTable -> Either SemanticError SymbolTable
checkVariable var st = 
    let inScope s = if symbolIsInScope s st then Right st 
                    else Left (SemanticError NotDeclaredError s) in
        case var of
            Name s -> inScope s
            NameSubscription s e -> inScope s >> checkExpression e st

checkSemantics :: Program -> Either SemanticError ()
checkSemantics = void . flip walkProgram (emptySymbolTable Nothing)
