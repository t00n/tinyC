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

data ErrorType = NotDeclaredError | NotAFunctionError | NameExistsError | NotAnArrayError | NotAScalarError
    deriving (Eq, Show)

data SemanticError = SemanticError {
    errorType :: ErrorType,
    errorVariable :: String
} deriving (Eq, Show)

kindToError :: Kind -> ErrorType
kindToError VariableKind = NotAScalarError
kindToError FunctionKind = NotAFunctionError
kindToError ArrayKind = NotAnArrayError

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

symbolIsSameLevel :: String -> SymbolTable -> Bool
symbolIsSameLevel n st = Map.member n (symbols st)

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

checkNameExistsBeforeInsert :: Name -> Type -> Kind -> SymbolTable -> Either SemanticError SymbolTable
checkNameExistsBeforeInsert n t k st = if symbolIsSameLevel (nameString n) st 
                                           then Left (SemanticError NameExistsError (nameString n))
                                       else Right $ insertSymbol (nameString n) (Info t k) st

checkNameInScope :: Name -> SymbolTable -> Either SemanticError SymbolTable
checkNameInScope name st = 
    let n = nameString name in
        if symbolIsInScope n st then Right st 
        else Left (SemanticError NotDeclaredError n)

checkNameIsKind :: Name -> Kind -> SymbolTable -> Either SemanticError SymbolTable
checkNameIsKind name kind st = 
    let n = nameString name in
        if symbolIsKind n kind st then Right st
        else Left (SemanticError (kindToError kind) n)
        
walkProgram :: Program -> SymbolTable -> Either SemanticError SymbolTable
walkProgram [] st = Right st
walkProgram (x:xs) st =
        case x of
            VarDeclaration t n _ -> checkNameExistsBeforeInsert n t (nameKind n) st >>= walkProgram xs  
            FuncDeclaration t n params stmt -> checkNameExistsBeforeInsert n t FunctionKind st >>= walkProgram xs >>= checkStatement stmt 

checkStatement :: Statement -> SymbolTable -> Either SemanticError SymbolTable
checkStatement stmt st = 
    case stmt of
        Assignment v e -> checkNameInScope v st >> checkExpression e st
        If e stmt1 -> checkExpression e st >> checkStatement stmt1 st
        IfElse e stmt1 stmt2 -> checkExpression e st >> checkStatement stmt1 st >> checkStatement stmt2 st
        While e stmt1 -> checkExpression e st >> checkStatement stmt1 st
        Return e -> checkExpression e st
        Block decl stmts -> walkProgram decl (emptySymbolTable $ Just st) >> checkStatements stmts st
        Write e -> checkExpression e st
        Read v -> checkNameInScope v st
        Expr e -> checkExpression e st

checkStatements :: [Statement] -> SymbolTable -> Either SemanticError SymbolTable
checkStatements = flip $ foldM $ flip checkStatement

checkExpression :: Expression -> SymbolTable -> Either SemanticError SymbolTable
checkExpression expr st = 
    case expr of 
        BinOp e1 _ e2 -> checkExpression e1 st >> checkExpression e2 st
        UnOp _ e -> checkExpression e st
        Call n params -> 
            checkNameInScope n st >> checkNameIsKind n FunctionKind st
            >> foldM (flip checkExpression) st params
        Length n -> checkNameInScope n st >> checkNameIsKind n ArrayKind st
        Var n -> checkNameInScope n st
        _ -> Right st

checkSemantics :: Program -> Either SemanticError ()
checkSemantics = void . flip walkProgram (emptySymbolTable Nothing)
