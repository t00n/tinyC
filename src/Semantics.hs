{-# LANGUAGE DeriveAnyClass, FlexibleInstances #-}

module Semantics (checkSemantics, symbolTable) where

import Control.Monad.Trans.Except (ExceptT(..), runExceptT, throwE)
import Control.Monad.State (State(..), get, put, modify, runState)
import Data.Maybe (isJust, fromJust)
import Data.Char (ord)
import Debug.Trace (traceShow)

import Parser
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
                newstmt <- check stmt
                newps <- check ps
                return $ FuncDeclaration t n newps newstmt

        in
        do
            mapM check ds
            mapM (\x -> if isFuncDecl x then checkFunc x else return x) ds
            return ds

consumeST :: SymbolTable -> SymbolTable
consumeST st = 
    let next = nextDF st
    in
    if next /= Nothing
        then fromJust next
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
            If e stmt1 -> check e >> checkExpressionIsScalar e >> check stmt1 >> return stmt
            IfElse e stmt1 stmt2 -> check e >> checkExpressionIsScalar e >> check stmt1 >> check stmt2 >> return stmt
            While e stmt1 -> check e >> checkExpressionIsScalar e >> check stmt1 >> return stmt
            Return e -> check e >> checkExpressionIsScalar e >> return stmt
            Block decl stmts -> do
                modify consumeST
                check decl >> check stmts >> return stmt
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

instance Checkable Parameter where
    check p@(Parameter t n) = checkNameNotDeclared n >> return p

checkNameNotDeclared :: Name -> ESSS ()
checkNameNotDeclared n = do
    st <- get
    if nameInParent (nameToString n) st
        then throwE (SemanticError NameExistsWarning (nameToString n))
    else return ()

checkNameDeclared :: Name -> ESSS ()
checkNameDeclared n = do
    st <- get
    if not (nameInScope (nameToString n) st) then throwE (SemanticError NotDeclaredError (nameToString n))
    else return ()

checkNameIsScalarity :: Name -> SymbolScalarity -> ESSS ()
checkNameIsScalarity n kind = do
    s <- getNameScalarity n
    if s /= kind then throwE (SemanticError (scalarityError kind) (nameToString n))
    else return ()

checkNameIsArray :: Name -> ESSS ()
checkNameIsArray n = do
    st <- get
    let res = unsafeSymbolIsScalarity (nameToString n) Array st 
    if not res then throwE (SemanticError NotAnArrayError (nameToString n))
    else return ()

checkNameIsFunction :: Name -> ESSS ()
checkNameIsFunction n = do
    st <- get
    let s = nameToString n
    let info = unsafeGetSymbolInfo s st 
    case info of
        (VarInfo _ _ _) -> throwE (SemanticError NotAFunctionError s)
        (FuncInfo _ _) -> return ()

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

checkArgument :: Expression -> SymbolInfo -> ESSS ()
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
    let funcName = nameToString func
    let params = (infoParams ... unsafeGetSymbolInfo) funcName st 
    foldl (>>) (return ()) $ zipWith checkArgument args params

entryPointExists :: Program -> ESSS Program
entryPointExists ds = 
    let isEntryPoint (FuncDeclaration IntType (Name "tiny") [] _) = True
        isEntryPoint _ = False
        funcs = filter (isEntryPoint) ds
    in
    do
        st <- get
        if length funcs == 0 then throwE (SemanticError NoTinyFunctionError "")
        else return ds

-- Helpers
expressionIsScalar :: Expression -> ESSS Bool
expressionIsScalar e = getExpressionScalarity e >>= (return . ((==) (Scalar)))

getNameScalarity :: Name -> ESSS SymbolScalarity
getNameScalarity n = do
    st <- get
    let s = unsafeSymbolScalarity (nameToString n) st
    case n of
        (Name _) -> return s
        (NameSubscription _ _) -> 
            if s == Scalar then throwE (SemanticError NotAnArrayError (show n))
            else return Scalar
            
getExpressionScalarity :: Expression -> ESSS SymbolScalarity
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

-- API

run ::Program -> (Either SemanticError Program, SymbolTable)
run prog = (runState . runExceptT) (do
    st <- ExceptT $ return $ constructST prog
    put st
    entryPointExists prog >>= check)
    (zipper emptyST)
    --runState (runExceptT (entryPointExists prog >>= check)) (zipper st)

checkSemantics :: Program -> (Either SemanticError Program)
checkSemantics = fst . run

symbolTable :: Program -> SymbolTable
symbolTable = root . snd . run
