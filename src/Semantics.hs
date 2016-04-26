module Semantics (checkSemantics, symbolTable, SemanticError(..), ErrorType(..), SymbolTable(..)) where

import Control.Monad (void, foldM)
import Control.Monad.State (runState, State(..))
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, fromJust)
import Data.Char (ord)

import Parser
import Utility

-- Symbols data types
data Scalarity = Scalar | Array
    deriving (Eq, Show)

data Info = VarInfo {
    infoType :: Type,
    infoScalarity :: Scalarity,
    infoSize :: Int
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

unsafeSymbolType :: String -> SymbolTable -> Type
unsafeSymbolType = infoType ... unsafeGetSymbolInfo

unsafeSymbolScalarity :: String -> SymbolTable -> Scalarity
unsafeSymbolScalarity = infoScalarity ... unsafeGetSymbolInfo

unsafeSymbolIsScalarity :: String -> Scalarity -> SymbolTable -> Bool
unsafeSymbolIsScalarity s k st = (unsafeSymbolScalarity s st) == k

unsafeSymbolIsType :: String -> Type -> SymbolTable -> Bool
unsafeSymbolIsType s t st = (unsafeSymbolType s st) == t

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
               | NotConstantSizeArrayError
    deriving (Eq, Show)

data SemanticError = SemanticError {
    errorType :: ErrorType,
    errorVariable :: String
} deriving (Eq, Show)

scalarityError :: Scalarity -> ErrorType
scalarityError Scalar = NotAScalarError
scalarityError Array = NotAnArrayError

infoError :: Info -> ErrorType
infoError (VarInfo _ k _) = scalarityError k
infoError (FuncInfo _ _) = NotAFunctionError

-- Check functions
class Checkable a where
    check :: a -> SymbolTable -> Either SemanticError SymbolTable

instance Checkable a => Checkable [a] where
    check = flip $ foldM $ flip check

instance Checkable Declaration where
    check x st = let paramToInfo (Parameter t (Name _)) = VarInfo t Scalar 1
                     paramToInfo (Parameter t (NameSubscription _ (Int i))) = VarInfo t Array i
                     paramToInfo (Parameter t (NameSubscription _ (Char c))) = VarInfo t Array (ord c)
                     paramToSymbol p@(Parameter _ n) = (n, paramToInfo p) in
        case x of
            VarDeclaration t n@(Name _) _ -> declareName n (VarInfo t Scalar 1) st
            VarDeclaration t n@(NameSubscription _ (Int i)) _ -> declareName n (VarInfo t Array i) st
            VarDeclaration t n@(NameSubscription _ (Char c)) _ -> declareName n (VarInfo t Array (ord c)) st
            VarDeclaration _ n _ -> Left $ SemanticError NotConstantSizeArrayError (show n)
            FuncDeclaration t n params stmt -> declareName n (FuncInfo t (map paramToInfo params)) st >>= declareNames (map paramToSymbol params) . emptySymbolTable . Just >>= check stmt

instance Checkable Statement where
    check stmt st = 
        case stmt of
            Assignment v e -> checkNameDeclared v st >>= check e >>= checkAssignment v e
            If e stmt1 -> check e st >>= checkExpressionIsScalar e >>= check stmt1
            IfElse e stmt1 stmt2 -> check e st >>= checkExpressionIsScalar e >>= check stmt1 >>= check stmt2
            While e stmt1 -> check e st >>= checkExpressionIsScalar e >>= check stmt1
            Return e -> check e st >>= checkExpressionIsScalar e
            Block decl stmts -> check decl st >>= check stmts
            Write e -> check e st >>= checkExpressionIsScalar e
            Read v -> checkNameDeclared v st >>= checkNameIsScalarity v Scalar
            Expr e -> check e st

instance Checkable Expression where
    check expr st = 
        case expr of 
            BinOp e1 _ e2 -> check e1 st 
                        >>= check e2 
                        >>= checkExpressionIsScalar e1 
                        >>= checkExpressionIsScalar e2
            UnOp _ e -> check e st >>= checkExpressionIsScalar e
            Call n args -> checkNameDeclared n st 
                        >>= checkNameIsFunction n
                        >>= check args
                        >>= checkArguments args n
            Length n -> checkNameDeclared n st >>= checkNameIsScalarity n Array
            Var name -> checkNameDeclared name st >>
                case name of
                    (NameSubscription n e) -> checkNameIsArray name st >>= check e >>= checkExpressionIsScalar e
                    _ -> return st
            _ -> Right st

checkNameNotDeclared :: Name -> SymbolTable -> Either SemanticError SymbolTable
checkNameNotDeclared n st = 
    if nameInBlock n st
        then Left (SemanticError NameExistsError (nameString n))
    else if nameInScope n st
        then Left (SemanticError NameExistsWarning (nameString n))
    else Right st

checkNameDeclared :: Name -> SymbolTable -> Either SemanticError SymbolTable
checkNameDeclared name st = 
    if nameInScope name st then Right st 
    else Left (SemanticError NotDeclaredError (nameString name))

checkNameIsScalarity :: Name -> Scalarity -> SymbolTable -> Either SemanticError SymbolTable
checkNameIsScalarity name kind st = 
    case getNameScalarity name st of
        Right x -> if x == kind then Right st else Left $ SemanticError (scalarityError kind) (nameString name)
        Left x -> Left x

checkNameIsArray :: Name -> SymbolTable -> Either SemanticError SymbolTable
checkNameIsArray name st = let res = unsafeSymbolIsScalarity (nameString name) Array st in
    if res then Right st else Left $ SemanticError NotAnArrayError (nameString name)

checkNameIsFunction :: Name -> SymbolTable -> Either SemanticError SymbolTable
checkNameIsFunction name st = 
    let n = nameString name
        info = unsafeGetSymbolInfo n st in
    case info of
        (VarInfo _ _ _) -> Left (SemanticError NotAFunctionError n)
        (FuncInfo _ _) -> Right st

getNameScalarity :: Name -> SymbolTable -> Either SemanticError Scalarity
getNameScalarity name st = 
    let s = unsafeSymbolScalarity (nameString name) st in
    case name of
        (Name _) -> Right $ if s == Scalar then Scalar else Array
        (NameSubscription _ _) -> if s == Scalar then Left $ SemanticError NotAnArrayError (show name) else Right Scalar

checkExpressionIsScalar :: Expression -> SymbolTable -> Either SemanticError SymbolTable
checkExpressionIsScalar expr st = 
    if expressionIsScalar expr st
        then Right st
    else
        Left $ SemanticError NotAScalarError $ show expr

checkAssignment :: Name -> Expression -> SymbolTable -> Either SemanticError SymbolTable
checkAssignment name expr st = do
    s1 <- getNameScalarity name st
    s2 <- getExpressionScalarity expr st
    if s1 /= s2
        then Left $ SemanticError NotSameScalarityError (show name ++ " " ++ show expr)
    else
        Right st

checkArguments :: [Expression] -> Name -> SymbolTable -> Either SemanticError SymbolTable
checkArguments args func st = 
    let funcName = nameString func
        params = (infoParams ... unsafeGetSymbolInfo) funcName st 
        comp arg param = 
            if expressionIsScalar arg st && infoScalarity param /= Scalar
                then Left $ SemanticError NotAnArrayError $ show arg
            else if (not $ expressionIsScalar arg st) && infoScalarity param /= Array
                then Left $ SemanticError NotAScalarError $ show arg
            else Right st in
    foldl (>>) (Right st) $ zipWith comp args params

getExpressionScalarity :: Expression -> SymbolTable -> Either SemanticError Scalarity
getExpressionScalarity expr st = 
    case expr of 
        (BinOp e1 _ e2) -> do
            s1 <- getExpressionScalarity e1 st
            s2 <- getExpressionScalarity e2 st
            if s1 /= s2
                then Left $ SemanticError NotSameScalarityError (show e1 ++ " " ++ show e2)
            else
                Right s1
        (UnOp _ e) -> getExpressionScalarity e st
        (Var name) -> getNameScalarity name st
        _ -> Right Scalar

-- Helpers
nameInScope :: Name -> SymbolTable -> Bool
nameInScope n = (||) <$> nameInBlock n <*> variableInParent n
    where variableInParent _ (SymbolTable _ Nothing) = False
          variableInParent n (SymbolTable s (Just p)) = nameInScope n p

nameInBlock :: Name -> SymbolTable -> Bool
nameInBlock n st = Map.member (nameString n) (symbols st)

expressionIsScalar :: Expression -> SymbolTable -> Bool
expressionIsScalar = ((==) (Right Scalar)) ... getExpressionScalarity

declareName :: Name -> Info -> SymbolTable -> Either SemanticError SymbolTable
declareName name info st = checkNameNotDeclared name st >>= return . insertSymbol (nameString name) info

declareNames :: [(Name, Info)] -> SymbolTable -> Either SemanticError SymbolTable
declareNames xs = flip (foldl (\acc x -> acc >>= (uncurry declareName) x )) xs . return

-- API

runCheck :: Program -> Either SemanticError SymbolTable
runCheck = flip check (emptySymbolTable Nothing)

checkSemantics :: Program -> Either SemanticError ()
checkSemantics = void . runCheck

symbolTable :: Program -> Either SemanticError SymbolTable
symbolTable = runCheck
