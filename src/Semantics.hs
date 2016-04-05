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

unsafeSymbolType :: String -> SymbolTable -> Type
unsafeSymbolType = infoType ... unsafeGetSymbolInfo

unsafeSymbolScalarity :: String -> SymbolTable -> Scalarity
unsafeSymbolScalarity = infoScalarity ... unsafeGetSymbolInfo

unsafeSymbolIsScalarity :: String -> Scalarity -> SymbolTable -> Bool
unsafeSymbolIsScalarity s k st = (unsafeSymbolScalarity s st) == k

unsafeSymbolIsType :: String -> Type -> SymbolTable -> Bool
unsafeSymbolIsType s t st = (unsafeSymbolType s st) == t

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

scalarityError :: Scalarity -> ErrorType
scalarityError Scalar = NotAScalarError
scalarityError Array = NotAnArrayError

infoError :: Info -> ErrorType
infoError (VarInfo _ k) = scalarityError k
infoError (FuncInfo _ _) = NotAFunctionError

-- Check functions
class Checkable a where
    check :: a -> SymbolTable -> Either SemanticError SymbolTable

instance Checkable a => Checkable [a] where
    check x st = foldM (flip check) st x

instance Checkable Declaration where
    check x st = let paramToInfo (Parameter t n) = VarInfo t (nameScalarity n) in
        case x of
            VarDeclaration t n _ -> declareName n (VarInfo t (nameScalarity n)) st
            FuncDeclaration t n params stmt -> declareName n (FuncInfo t (map paramToInfo params)) st >>= check stmt 

instance Checkable Statement where
    check stmt st = 
        case stmt of
            Assignment v e -> checkNameDeclared v st >>= check e
            If e stmt1 -> check e st >>= check stmt1
            IfElse e stmt1 stmt2 -> check e st >>= check stmt1 >>= check stmt2
            While e stmt1 -> check e st >>= check stmt1
            Return e -> check e st
            Block decl stmts -> check decl (emptySymbolTable $ Just st) >>= check stmts
            Write e -> check e st
            Read v -> checkNameDeclared v st
            Expr e -> check e st

instance Checkable Expression where
    check expr st = 
        case expr of 
            BinOp e1 _ e2 -> check e1 st >>= check e2
            UnOp _ e -> check e st
            Call n params -> checkNameDeclared n st 
            -- >> nameIsScalarity n FunctionScalarity st
            --    >> foldM (flip check) st params
            Length n -> checkNameDeclared n st >>= nameIsScalarity n Array
            Var n -> checkNameDeclared n st
            _ -> Right st

-- Helpers
nameInScope :: Name -> SymbolTable -> Bool
nameInScope n = (||) <$> nameInBlock n <*> variableInParent n
    where variableInParent _ (SymbolTable _ Nothing) = False
          variableInParent n (SymbolTable s (Just p)) = nameInScope n p

nameInBlock :: Name -> SymbolTable -> Bool
nameInBlock n st = Map.member (nameString n) (symbols st)

checkNameNotDeclared :: Name -> SymbolTable -> Either SemanticError SymbolTable
checkNameNotDeclared n st = 
    if nameInBlock n st
        then Left (SemanticError NameExistsError (nameString n))
    else if nameInScope n st
        then Left (SemanticError NameExistsWarning (nameString n))
    else Right st

declareName :: Name -> Info -> SymbolTable -> Either SemanticError SymbolTable
declareName name info st = checkNameNotDeclared name st >>= return . insertSymbol (nameString name) info

checkNameDeclared :: Name -> SymbolTable -> Either SemanticError SymbolTable
checkNameDeclared name st = 
    if nameInScope name st then Right st 
    else Left (SemanticError NotDeclaredError (nameString name))

nameIsScalarity :: Name -> Scalarity -> SymbolTable -> Either SemanticError SymbolTable
nameIsScalarity name kind st = 
    let n = nameString name in
        if unsafeSymbolIsScalarity n kind st then Right st
        else Left (SemanticError (scalarityError kind) n)


expressionIsScalar :: Expression -> SymbolTable -> Bool
expressionIsScalar expr st = 
    case expr of 
        BinOp e1 _ e2 -> expressionIsScalar e1 st && expressionIsScalar e2 st
        UnOp _ e -> expressionIsScalar e st
        Call _ _ -> True
        Length _ -> True
        Var name -> unsafeSymbolScalarity (nameString name) st == Scalar
        Int _ -> True
        Char _ -> True

expressionNamesExist :: Expression -> SymbolTable -> Either SemanticError SymbolTable
expressionNamesExist expr st = 
    case expr of
        BinOp e1 _ e2 -> expressionNamesExist e1 st >>= expressionNamesExist e2
        UnOp _ e -> expressionNamesExist e st
        Call n params -> checkNameDeclared n st
        Length n -> checkNameDeclared n st
        Var n -> checkNameDeclared n st
        _ -> Right st

--expressionCheckScalarity :: Expression -> SymbolTable -> Either SemanticError SymbolTable
--expressionCheckScalarity expr st = 
--    case expr of
--        BinOp e1 _ e2 -> 

runCheck :: Program -> Either SemanticError SymbolTable
runCheck = flip check (emptySymbolTable Nothing)

checkSemantics :: Program -> Either SemanticError ()
checkSemantics = void . runCheck

symbolTable :: Program -> Either SemanticError SymbolTable
symbolTable = runCheck
