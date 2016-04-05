module Semantics (checkSemantics, SemanticError(..), ErrorType(..), SymbolTable(..)) where

import Control.Monad (void, foldM)
import Control.Monad.State (runState, State(..))
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, fromJust)

import Parser

-- Symbols data types
data Scalarity = Scalar | Array
    deriving (Eq, Show)

data Info = VarInfo {
    infoType :: Type,
    infoScalarity :: Scalarity
}         | FuncInfo {
    infoType :: Type,
    infoParams :: [Info]
} deriving (Eq, Show)

type Symbols = Map.Map String Info

data SymbolTable = SymbolTable {
    symbols :: Symbols,
    parent :: Maybe SymbolTable
} deriving (Eq, Show)

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

(...) :: (Functor f, Functor f1) => (a -> b) -> f (f1 a) -> f (f1 b)
(...) = fmap . fmap

symbolType :: String -> SymbolTable -> Type
symbolType = infoType ... unsafeGetSymbolInfo

symbolScalarity :: String -> SymbolTable -> Scalarity
symbolScalarity = infoScalarity ... unsafeGetSymbolInfo

symbolIsScalarity :: String -> Scalarity -> SymbolTable -> Bool
symbolIsScalarity s k st = (symbolScalarity s st) == k

symbolIsType :: String -> Type -> SymbolTable -> Bool
symbolIsType s t st = (symbolType s st) == t

nameString :: Name -> String
nameString (Name s) = s
nameString (NameSubscription s _) = s

nameScalarity :: Name -> Scalarity
nameScalarity (Name _) = Scalar
nameScalarity (NameSubscription _ _) = Array

-- Errors data types
data ErrorType = NotDeclaredError 
               | NotAFunctionError 
               | NameExistsError 
               | NotAnArrayError 
               | NotAScalarError
               | NotSameScalarityError
               | NameExistsWarning
    deriving (Eq, Show)

data SemanticError = SemanticError {
    errorType :: ErrorType,
    errorVariable :: String
} deriving (Eq, Show)

kindToError :: Scalarity -> ErrorType
kindToError Scalar = NotAScalarError
kindToError Array = NotAnArrayError

infoToError :: Info -> ErrorType
infoToError (VarInfo _ k) = kindToError k
infoToError (FuncInfo _ _) = NotAFunctionError

-- Declarations
symbolIsInScope :: String -> SymbolTable -> Bool
symbolIsInScope n st = Map.member n (symbols st) || variableInParent n st
    where variableInParent _ (SymbolTable _ Nothing) = False
          variableInParent n (SymbolTable s (Just p)) = symbolIsInScope n p

symbolIsInBlock :: String -> SymbolTable -> Bool
symbolIsInBlock n st = Map.member n (symbols st)

checkNameExists :: String -> SymbolTable -> Either SemanticError SymbolTable
checkNameExists name st = 
    if symbolIsInBlock name st
        then Left (SemanticError NameExistsError name)
    else if symbolIsInScope name st
        then Left (SemanticError NameExistsWarning name)
    else Right st

insertVariable :: String -> Type -> Scalarity -> SymbolTable -> Either SemanticError SymbolTable
insertVariable name t k st = checkNameExists name st >>= return . insertSymbol name (VarInfo t k)


insertFunction :: String -> Type -> [Info] -> SymbolTable -> Either SemanticError SymbolTable
insertFunction name ret params st = checkNameExists name st >>= return . insertSymbol name (FuncInfo ret params)

checkDeclarations :: [Declaration] -> SymbolTable -> Either SemanticError SymbolTable
checkDeclarations [] st = Right st
checkDeclarations (x:xs) st =
    let paramToInfo (Parameter t n) = VarInfo t (nameScalarity n) in
        case x of
            VarDeclaration t n _ -> insertVariable (nameString n) t (nameScalarity n) st >>= checkDeclarations xs  
            FuncDeclaration t n params stmt -> insertFunction (nameString n) t (map paramToInfo params) st >>= checkDeclarations xs >>= checkStatement stmt 

checkNameInScope :: Name -> SymbolTable -> Either SemanticError SymbolTable
checkNameInScope name st = 
    let n = nameString name in
        if symbolIsInScope n st then Right st 
        else Left (SemanticError NotDeclaredError n)

checkNameIsScalarity :: Name -> Scalarity -> SymbolTable -> Either SemanticError SymbolTable
checkNameIsScalarity name kind st = 
    let n = nameString name in
        if symbolIsScalarity n kind st then Right st
        else Left (SemanticError (kindToError kind) n)

checkStatement :: Statement -> SymbolTable -> Either SemanticError SymbolTable
checkStatement stmt st = 
    case stmt of
        Assignment v e -> checkNameInScope v st >> checkExpression e st
        If e stmt1 -> checkExpression e st >> checkStatement stmt1 st
        IfElse e stmt1 stmt2 -> checkExpression e st >> checkStatement stmt1 st >> checkStatement stmt2 st
        While e stmt1 -> checkExpression e st >> checkStatement stmt1 st
        Return e -> checkExpression e st
        Block decl stmts -> checkDeclarations decl (emptySymbolTable $ Just st) >> checkStatements stmts st
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
        Call n params -> checkNameInScope n st 
        -- >> checkNameIsScalarity n FunctionScalarity st
        --    >> foldM (flip checkExpression) st params
        Length n -> checkNameInScope n st >> checkNameIsScalarity n Array st
        Var n -> checkNameInScope n st
        _ -> Right st

expressionIsScalar :: Expression -> SymbolTable -> Bool
expressionIsScalar expr st = 
    case expr of 
        BinOp e1 _ e2 -> expressionIsScalar e1 st && expressionIsScalar e2 st
        UnOp _ e -> expressionIsScalar e st
        Call _ _ -> True
        Length _ -> True
        Var name -> symbolScalarity (nameString name) st == Scalar
        Int _ -> True
        Char _ -> True

expressionNamesExist :: Expression -> SymbolTable -> Either SemanticError SymbolTable
expressionNamesExist expr st = 
    case expr of
        BinOp e1 _ e2 -> expressionNamesExist e1 st >>= expressionNamesExist e2
        UnOp _ e -> expressionNamesExist e st
        Call n params -> checkNameInScope n st
        Length n -> checkNameInScope n st
        Var n -> checkNameInScope n st
        _ -> Right st

--expressionCheckScalarity :: Expression -> SymbolTable -> Either SemanticError SymbolTable
--expressionCheckScalarity expr st = 
--    case expr of
--        BinOp e1 _ e2 -> 

checkSemantics :: Program -> Either SemanticError ()
checkSemantics = void . flip checkDeclarations (emptySymbolTable Nothing)

symbolTable :: Program -> Either SemanticError SymbolTable
symbolTable = flip checkDeclarations (emptySymbolTable Nothing)
