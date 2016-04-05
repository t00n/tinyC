module Semantics (checkSemantics, SemanticError(..), ErrorType(..)) where

import Control.Monad (void, foldM)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, fromJust)

import Parser

data Kind = VariableKind | ArrayKind
    deriving (Eq, Show)

data Info = VarInfo {
    infoType :: Type,
    infoKind :: Kind
}         | FuncInfo {
    infoType :: Type,
    infoParams :: [Info]
} deriving (Eq, Show)

type Symbols = Map.Map String Info

data SymbolTable = SymbolTable {
    symbols :: Symbols,
    parent :: Maybe SymbolTable
} deriving (Eq, Show)

data ErrorType = NotDeclaredError 
               | NotAFunctionError 
               | NameExistsError 
               | NotAnArrayError 
               | NotAScalarError
               | NotSameKindError
               | NameExistsWarning
    deriving (Eq, Show)

data SemanticError = SemanticError {
    errorType :: ErrorType,
    errorVariable :: String
} deriving (Eq, Show)

kindToError :: Kind -> ErrorType
kindToError VariableKind = NotAScalarError
kindToError ArrayKind = NotAnArrayError

infoToError :: Info -> ErrorType
infoToError (VarInfo _ k) = kindToError k
infoToError (FuncInfo _ _) = NotAFunctionError

emptySymbolTable :: Maybe SymbolTable -> SymbolTable
emptySymbolTable = SymbolTable Map.empty

insertSymbol :: String -> Info -> SymbolTable -> SymbolTable
insertSymbol s i = SymbolTable <$> Map.insert s i . symbols <*> parent

getSymbolInfo :: String -> SymbolTable -> Maybe Info
getSymbolInfo s st = let res = Map.lookup s (symbols st) in 
                         if res == Nothing then parent st >>= getSymbolInfo s
                         else res

unsafeGetSymbolInfo :: String -> SymbolTable -> Info
unsafeGetSymbolInfo s st = let info = getSymbolInfo s st in
    if isJust info then fromJust info
    else error ("Symbol " ++ s ++ " is not in symbol table " ++ show st)


symbolIsInScope :: String -> SymbolTable -> Bool
symbolIsInScope n st = Map.member n (symbols st) || variableInParent n st
    where variableInParent _ (SymbolTable _ Nothing) = False
          variableInParent n (SymbolTable s (Just p)) = symbolIsInScope n p

symbolIsInBlock :: String -> SymbolTable -> Bool
symbolIsInBlock n st = Map.member n (symbols st)

(...) :: (Functor f, Functor f1) => (a -> b) -> f (f1 a) -> f (f1 b)
(...) = fmap . fmap

symbolType :: String -> SymbolTable -> Type
symbolType = infoType ... unsafeGetSymbolInfo

symbolKind :: String -> SymbolTable -> Kind
symbolKind = infoKind ... unsafeGetSymbolInfo

symbolIsKind :: String -> Kind -> SymbolTable -> Bool
symbolIsKind s k st = (symbolKind s st) == k

symbolIsType :: String -> Type -> SymbolTable -> Bool
symbolIsType s t st = (symbolType s st) == t

nameString :: Name -> String
nameString (Name s) = s
nameString (NameSubscription s _) = s

nameKind :: Name -> Kind
nameKind (NameSubscription _ _) = ArrayKind
nameKind (Name _) = VariableKind

checkNameExists :: String -> SymbolTable -> Either SemanticError SymbolTable
checkNameExists name st = 
    if symbolIsInBlock name st
        then Left (SemanticError NameExistsError name)
    else if symbolIsInScope name st
        then Left (SemanticError NameExistsWarning name)
    else Right st

insertVariable :: String -> Type -> Kind -> SymbolTable -> Either SemanticError SymbolTable
insertVariable name t k st = checkNameExists name st >>= return . insertSymbol name (VarInfo t k)


insertFunction :: String -> Type -> [Info] -> SymbolTable -> Either SemanticError SymbolTable
insertFunction name ret params st = checkNameExists name st >>= return . insertSymbol name (FuncInfo ret params)

walkProgram :: Program -> SymbolTable -> Either SemanticError SymbolTable
walkProgram [] st = Right st
walkProgram (x:xs) st =
    let paramToInfo (Parameter t n) = VarInfo t (nameKind n) in
        case x of
            VarDeclaration t n _ -> insertVariable (nameString n) t (nameKind n) st >>= walkProgram xs  
            FuncDeclaration t n params stmt -> insertFunction (nameString n) t (map paramToInfo params) st >>= walkProgram xs >>= checkStatement stmt 

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
        --Call n params -> checkNameInScope n st >> checkNameIsKind n FunctionKind st
        --    >> foldM (flip checkExpression) st params
        Length n -> checkNameInScope n st >> checkNameIsKind n ArrayKind st
        Var n -> checkNameInScope n st
        _ -> Right st

expressionKind :: Expression -> SymbolTable -> Either SemanticError Kind
expressionKind expr st = 
    case expr of 
        BinOp e1 _ e2 -> do
            k1 <- expressionKind e1 st
            k2 <- expressionKind e2 st
            if k1 /= k2 
                then Left (SemanticError NotSameKindError "todo")
            else Right k1
        UnOp _ e -> expressionKind e st
        Call n _ -> Right $ (symbolKind . nameString) n st
        Length n -> Right $ (symbolKind . nameString) n st
        Var n -> Right $ (symbolKind . nameString) n st
        Int x -> Right VariableKind
        Char x -> Right VariableKind

checkSemantics :: Program -> Either SemanticError SymbolTable
checkSemantics = flip walkProgram (emptySymbolTable Nothing)
