{-# LANGUAGE DeriveAnyClass, FlexibleInstances #-}

module Semantics (checkSemantics, symbolTable) where

import Control.Monad.Trans.Except (ExceptT(..), runExceptT, throwE)
import Control.Monad.State (State(..), get, put, modify, runState, gets)
import Control.Monad (foldM)
import Data.Maybe (isJust, fromJust)
import Data.Char (ord)
import qualified Data.Map as M
import Debug.Trace (traceShow, trace)
import Data.Tree (drawTree)

import AST
import Utility
import SemanticError
import SymbolTable

-- Symbols data types
type ESSS = ExceptT SemanticError (State SymbolTable)

-- Check functions
class Checkable a where
    check :: a -> ESSS a

instance Checkable [Statement] where
    check = mapM check

instance Checkable [Expression] where
    check = mapM check

instance Checkable [Parameter] where
    check = mapM check

instance Checkable [Declaration] where
    check ds = 
        let isFuncDecl (FuncDeclaration _ _ _ _) = True
            isFuncDecl _ = False
            checkFunc (FuncDeclaration t n ps stmt) = do
                consumeST
                newstmt <- check stmt
                newps <- check ps
                return $ FuncDeclaration t n newps newstmt
        in
        do
            mapM check ds
            mapM (\x -> if isFuncDecl x then checkFunc x else return x) ds
            return ds

consumeST :: ESSS ()
consumeST = do
    next <- gets nextDF
    if next /= Nothing
        then put $ fromJust next
    else error "No symbol table anymore ????"

instance Checkable Declaration where
    check d = 
        case d of
            VarDeclaration _ n _ -> checkNameNotDeclared n >> return d
            FuncDeclaration t n ps stmt -> checkNameNotDeclared n >> return d

instance Checkable Statement where
    check stmt = 
        case stmt of
            Assignment v e -> checkNameDeclared v >> check e >> checkAssignment v e >> return stmt
            If e stmt1 -> check e >> checkExpressionIsValue e >> check stmt1 >> return stmt
            IfElse e stmt1 stmt2 ->  check e >> checkExpressionIsValue e >> check stmt1 >> check stmt2 >> return stmt
            While e stmt1 -> check e >> checkExpressionIsValue e >> check stmt1 >> return stmt
            Return e -> check e >> checkExpressionIsValue e >> return stmt
            Block decl stmts -> check decl >> check stmts >> return stmt
            Write e -> check e >> checkExpressionIsValue e >> return stmt
            Read v -> checkNameDeclared v >> checkNameIsValue v >> return stmt
            Expr e -> check e >> return stmt

instance Checkable Expression where
    check expr = do
        st <- get
        case expr of 
            BinOp e1 op e2 -> check e1
                        >> check e2
                        >> checkBinOp e1 op e2
                        >> return expr
            UnOp _ e -> check e >> checkExpressionIsValue e
                        >> return expr
            Call n args -> checkNameDeclared n
                        >> checkNameIsFunction n
                        >> check args
                        >> checkArguments args n
                        >> return expr
            Length n -> checkNameDeclared n >> checkNameIsArray n
                        >> return expr
            Var name -> checkNameDeclared name >>
                case name of
                    (NamePointer n) -> checkNameIsValue name >> return expr
                    (NameSubscription n e) -> checkNameIsValue name >> mapM check e >> mapM checkExpressionIsValue e >> return expr
                    _ -> return expr
            Address name -> checkNameDeclared name >> checkNameIsValue name >> return expr
            _ -> return expr

instance Checkable Parameter where
    check p@(Parameter t n) = checkNameNotDeclared n >> return p

checkNameNotDeclared :: Name -> ESSS ()
checkNameNotDeclared n = do
    st <- get
    if nameInParent (nameToString n) st
        then throwE (SemanticError NameExistsWarning (show n))
    else return ()

checkNameDeclared :: Name -> ESSS ()
checkNameDeclared n = do
    st <- get
    if not (nameInScope (nameToString n) st) then throwE (SemanticError NotDeclaredError (show n))
    else return ()

checkBinOp :: Expression -> BinaryOperator -> Expression -> ESSS ()
checkBinOp e1 op e2 = do
    k1 <- getExpressionKind e1
    k2 <- getExpressionKind e2
    let checkPlus = if k1 == Array   && k2 /= Value then throwE (SemanticError NotAValueError (show e2))
               else if k1 == Pointer && k2 /= Value then throwE (SemanticError NotAValueError (show e2))
               else if k2 == Array   && k1 /= Value then throwE (SemanticError NotAValueError (show e2))
               else if k2 == Pointer && k1 /= Value then throwE (SemanticError NotAValueError (show e2))
               else return ()
    let checkMinus = if k1 == Value && k2 /= Value then throwE (SemanticError NotAValueError (show e2))
                else return ()
    let checkTimesAndDivide = if k1 /= Value then throwE (SemanticError NotAValueError (show e1))
                         else if k2 /= Value then throwE (SemanticError NotAValueError (show e2))
                         else return ()
    case op of
        Plus -> checkPlus
        Minus -> checkMinus
        Times -> checkTimesAndDivide
        Divide -> checkTimesAndDivide
        _ -> return ()

checkNameIsValue :: Name -> ESSS ()
checkNameIsValue n = do
    k <- getNameKind n
    if k /= Value then throwE (SemanticError NotAValueError (show n))
    else return ()

checkNameIsArray :: Name -> ESSS ()
checkNameIsArray n = do
    k <- getNameKind n
    if k /= Array then throwE (SemanticError NotAnArrayError (show n))
    else return ()

checkNameIsPointer :: Name -> ESSS ()
checkNameIsPointer n = do
    k <- getNameKind n
    if k /= Pointer then throwE (SemanticError NotAPointerError (show n))
    else return ()

checkNameIsFunction :: Name -> ESSS ()
checkNameIsFunction n = do
    st <- get
    let k = nameToString n
    let info = unsafeGetSymbolInfo k st 
    case info of
        (VarInfo _ _ _) -> throwE (SemanticError NotAFunctionError (show n))
        (FuncInfo _ _) -> return ()

checkExpressionIsValue :: Expression -> ESSS ()
checkExpressionIsValue expr = do
    k <- getExpressionKind expr
    if k /= Value
        then throwE (SemanticError NotAValueError (show expr))
    else return ()

checkExpressionIsArray :: Expression -> ESSS ()
checkExpressionIsArray expr = do
    k <- getExpressionKind expr
    if k /= Array
        then throwE (SemanticError NotAnArrayError (show expr))
    else return ()

checkExpressionIsPointer :: Expression -> ESSS ()
checkExpressionIsPointer expr = do
    k <- getExpressionKind expr
    if k /= Pointer
        then throwE (SemanticError NotAPointerError (show expr))
    else return ()

checkAssignment :: Name -> Expression -> ESSS ()
checkAssignment name expr = do
    s1 <- getNameKind name
    if s1 == Array
        then throwE (SemanticError CantAssignArrayError (show name))
    else
        return ()

checkArgument :: Expression -> SymbolInfo -> ESSS ()
checkArgument arg param = do
    k <- getExpressionKind arg
    if infoKind param == Value && k /= Value
        then throwE (SemanticError NotAValueError $ show arg)
    else return ()

checkArguments :: [Expression] -> Name -> ESSS ()
checkArguments args func = do
    st <- get
    let funcName = nameToString func
    let params = map snd $ (infoParams ... unsafeGetSymbolInfo) funcName st 
    foldl (>>) (return ()) $ zipWith checkArgument args params

entryPointExists :: Program -> ESSS Program
entryPointExists ds = 
    let isEntryPoint (FuncDeclaration IntType (Name "tiny") [] _) = True
        isEntryPoint _ = False
        funcs = filter (isEntryPoint) ds
    in
    do
        if length funcs == 0 then throwE (SemanticError NoTinyFunctionError "")
        else return ds

-- Helpers
getNameKind :: Name -> ESSS SymbolKind
getNameKind n = do
    st <- get
    let kind = unsafeSymbolKind (nameToString n) st
    let ArraySize size = unsafeSymbolSize (nameToString n) st
    case n of
        (Name _) -> return kind
        (NameSubscription _ es) -> 
            case kind of 
                Value -> throwE (SemanticError NotAnArrayError (show n))
                Pointer -> if length es > 1 then throwE (SemanticError TooMuchSubscriptionError (show n)) else return Value
                Array -> if length es > length size then throwE (SemanticError TooMuchSubscriptionError (show n))
                         else if length es == length size then return Value
                         else return Array
        (NamePointer _) -> 
            case kind of
                Value -> throwE (SemanticError NotAPointerError (show n))
                Pointer -> return Value
                Array -> if length size > 1 then return Array
                         else return Value
            
getExpressionKind :: Expression -> ESSS SymbolKind
getExpressionKind expr = do
    st <- get
    case expr of 
        (BinOp e1 _ e2) -> return Value
        (UnOp _ e) -> getExpressionKind e
        (Var name) -> getNameKind name
        (Address _) -> return Pointer
        _ -> return Value

-- API

run :: Program -> (Either SemanticError Program, SymbolTable)
run prog = (runState . runExceptT) (do
        st <- ExceptT $ return $ constructST prog
        put st
        entryPointExists prog >>= check) (zipper emptyST)

checkSemantics :: Program -> (Either SemanticError Program)
checkSemantics = fst . run

symbolTable :: Program -> SymbolTable
symbolTable = root . snd . run
