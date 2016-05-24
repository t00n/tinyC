{-# LANGUAGE DeriveAnyClass #-}

module Semantics (checkSemantics, symbolTable, SemanticError(..), ErrorType(..), SymbolTable(..)) where

import Control.Monad.Trans.Except (ExceptT(..), runExceptT, throwE)
import Control.Monad.State (StateT(..), get, put, modify, runStateT)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, fromJust)
import Data.Char (ord)

import Parser
import Utility
import SemanticError

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
type ESSS = ExceptT SemanticError (StateT SymbolTable IO)

scalarityError :: Scalarity -> ErrorType
scalarityError Scalar = NotAScalarError
scalarityError Array = NotAnArrayError

infoError :: Info -> ErrorType
infoError (VarInfo _ k _) = scalarityError k
infoError (FuncInfo _ _) = NotAFunctionError

-- Check functions
class Checkable a where
    check :: a -> ESSS a

instance Checkable a => Checkable [a] where
    check = mapM check

instance Checkable Declaration where
    check x = let paramToInfo (Parameter t (Name _)) = VarInfo t Scalar 1
                  paramToInfo (Parameter t (NameSubscription _ (Int i))) = VarInfo t Array i
                  paramToInfo (Parameter t (NameSubscription _ (Char c))) = VarInfo t Array (ord c)
                  paramToSymbol p@(Parameter _ n) = (n, paramToInfo p) in
        case x of
            VarDeclaration t n@(Name _) _ -> declareName n (VarInfo t Scalar 1) >> return x
            VarDeclaration t n@(NameSubscription _ (Int i)) _ -> declareName n (VarInfo t Array i) >> return x
            VarDeclaration t n@(NameSubscription _ (Char c)) _ -> declareName n (VarInfo t Array (ord c)) >> return x
            VarDeclaration _ n _ -> throwE (SemanticError NotConstantSizeArrayError (show n)) >> return x
            FuncDeclaration t n params stmt -> do
                declareName n (FuncInfo t (map paramToInfo params))
                st <- get
                put $ emptySymbolTable $ Just st
                declareNames (map paramToSymbol params) >> check stmt >> return x

instance Checkable Statement where
    check stmt = 
        case stmt of
            Assignment v e -> checkNameDeclared v >> check e >> checkAssignment v e >> return stmt
            If e stmt1 -> check e >> checkExpressionIsScalar e >> check stmt1 >> return stmt
            IfElse e stmt1 stmt2 -> check e >> checkExpressionIsScalar e >> check stmt1 >> check stmt2 >> return stmt
            While e stmt1 -> check e >> checkExpressionIsScalar e >> check stmt1 >> return stmt
            Return e -> check e >> checkExpressionIsScalar e >> return stmt
            Block decl stmts -> check decl >> check stmts >> return stmt
            Write e -> check e >> checkExpressionIsScalar e >> return stmt
            Read v -> checkNameDeclared v >> checkNameIsScalarity v Scalar >> return stmt
            Expr e -> check e >> return stmt

instance Checkable Expression where
    check expr = 
        case expr of 
            BinOp e1 _ e2 -> check e1
                        >> check e2 
                        >> checkExpressionIsScalar e1 
                        >> checkExpressionIsScalar e2
                        >> return expr
            UnOp _ e -> check e >> checkExpressionIsScalar e
                        >> return expr
            Call n args -> checkNameDeclared n
                        >> checkNameIsFunction n
                        >> check args
                        >> checkArguments args n
                        >> return expr
            Length n -> checkNameDeclared n >> checkNameIsScalarity n Array
                        >> return expr
            Var name -> checkNameDeclared name >>
                case name of
                    (NameSubscription n e) -> checkNameIsArray name >> check e >> checkExpressionIsScalar e >> return expr
                    _ -> return expr
            _ -> return expr

checkNameNotDeclared :: Name -> ESSS ()
checkNameNotDeclared n = do
    st <- get
    if nameInBlock n st
        then throwE (SemanticError NameExistsError (nameString n))
    else if nameInScope n st
        then throwE (SemanticError NameExistsWarning (nameString n))
    else return ()

checkNameDeclared :: Name -> ESSS ()
checkNameDeclared name = do
    st <- get
    if not (nameInScope name st) then throwE (SemanticError NotDeclaredError (nameString name))
    else return ()

checkNameIsScalarity :: Name -> Scalarity -> ESSS ()
checkNameIsScalarity name kind = do
    s <- getNameScalarity name
    if s /= kind then throwE (SemanticError (scalarityError kind) (nameString name))
    else return ()

checkNameIsArray :: Name -> ESSS ()
checkNameIsArray name = do
    st <- get
    let res = unsafeSymbolIsScalarity (nameString name) Array st 
    if not res then throwE (SemanticError NotAnArrayError (nameString name))
    else return ()

checkNameIsFunction :: Name -> ESSS ()
checkNameIsFunction name = do
    st <- get
    let n = nameString name
    let info = unsafeGetSymbolInfo n st 
    case info of
        (VarInfo _ _ _) -> throwE (SemanticError NotAFunctionError n)
        (FuncInfo _ _) -> return ()

getNameScalarity :: Name -> ESSS Scalarity
getNameScalarity name = do
    st <- get
    let s = unsafeSymbolScalarity (nameString name) st
    case name of
        (Name _) -> return s
        (NameSubscription _ _) -> 
            if s == Scalar then throwE (SemanticError NotAnArrayError (show name)) >> return Scalar
            else return Scalar

checkExpressionIsScalar :: Expression -> ESSS ()
checkExpressionIsScalar expr = do
    st <- get
    s <- expressionIsScalar expr
    if not s
        then throwE (SemanticError NotAScalarError $ show expr)
    else return ()

checkAssignment :: Name -> Expression -> ESSS ()
checkAssignment name expr = do
    s1 <- getNameScalarity name
    s2 <- getExpressionScalarity expr
    if s1 /= s2
        then throwE (SemanticError NotSameScalarityError (show name ++ " " ++ show expr))
    else
        return ()

checkArgument :: Expression -> Info -> ESSS ()
checkArgument arg param = do
    st <- get
    s <- expressionIsScalar arg
    if s && infoScalarity param /= Scalar
        then throwE (SemanticError NotAnArrayError $ show arg)
    else if (not s) && infoScalarity param /= Array
        then throwE (SemanticError NotAScalarError $ show arg)
    else return ()

checkArguments :: [Expression] -> Name -> ESSS ()
checkArguments args func = do
    st <- get
    let funcName = nameString func
    let params = (infoParams ... unsafeGetSymbolInfo) funcName st 
    foldl (>>) (return ()) $ zipWith checkArgument args params

getExpressionScalarity :: Expression -> ESSS Scalarity
getExpressionScalarity expr = do
    st <- get
    case expr of 
        (BinOp e1 _ e2) -> do
            s1 <- getExpressionScalarity e1
            s2 <- getExpressionScalarity e2
            if s1 /= s2
                then throwE (SemanticError NotSameScalarityError (show e1 ++ " " ++ show e2)) >> return s1
            else
                return s1
        (UnOp _ e) -> getExpressionScalarity e
        (Var name) -> getNameScalarity name
        _ -> return Scalar

entryPointExists :: Program -> ESSS Program
entryPointExists ds = 
    let isEntryPoint (FuncDeclaration IntType (Name "tiny") [] _) = True
        isEntryPoint _ = False
        funcs = filter (isEntryPoint) ds
    in
    do
        st <- get
        if length funcs == 0 then throwE (SemanticError NoTinyFunctionError "") >> return ds
        else if length funcs > 1 then throwE (SemanticError SeveralTinyFunctionError "") >> return ds
        else return ds

-- Helpers
nameInScope :: Name -> SymbolTable -> Bool
nameInScope n = (||) <$> nameInBlock n <*> variableInParent n
    where variableInParent _ (SymbolTable _ Nothing) = False
          variableInParent n (SymbolTable s (Just p)) = nameInScope n p

nameInBlock :: Name -> SymbolTable -> Bool
nameInBlock n st = Map.member (nameString n) (symbols st)

expressionIsScalar :: Expression -> ESSS Bool
expressionIsScalar e = getExpressionScalarity e >>= (return . ((==) (Scalar)))

declareName :: Name -> Info -> ESSS ()
declareName name info = do
    st <- get
    checkNameNotDeclared name 
    modify $ insertSymbol (nameString name) info

declareNames :: [(Name, Info)] -> ESSS ()
declareNames = foldl (\acc x -> acc >> (uncurry declareName) x ) (return ())

-- API

run ::Program -> IO (Either SemanticError Program, SymbolTable)
run prog = runStateT (runExceptT (entryPointExists prog >>= check)) (emptySymbolTable Nothing)

checkSemantics :: Program -> IO (Either SemanticError Program)
checkSemantics = ((<$>) fst) . run

symbolTable :: Program -> IO SymbolTable
symbolTable = ((<$>) snd) . run
