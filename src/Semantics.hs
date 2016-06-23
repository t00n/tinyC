{-# LANGUAGE DeriveAnyClass, FlexibleInstances #-}

module Semantics (checkSemantics, symbolTable) where

import Control.Monad.Trans.Except (ExceptT(..), runExceptT, throwE)
import Control.Monad.State (State(..), get, put, modify, runState, gets)
import Data.Maybe (isJust, fromJust)
import Data.Char (ord)
import qualified Data.Map as M
import Debug.Trace (traceShow, trace)
import Data.Tree (drawTree)

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
            BinOp e1 _ e2 -> check e1
                        >> check e2
                        >> checkExpressionIsValue e1
                        >> checkExpressionIsValue e2
                        >> return expr
            UnOp _ e -> check e >> checkExpressionIsValue e
                        >> return expr
            Call n args -> checkNameDeclared n
                        >> checkNameIsFunction n
                        >> check args
                        >> checkArguments args n
                        >> return expr
            Length n -> checkNameDeclared n >> checkNameIsPointer n
                        >> return expr
            Var name -> checkNameDeclared name >>
                case name of
                    (NamePointer n) -> checkNameIsValue name >> return expr
                    (NameSubscription n e) -> checkNameIsValue name >> check e >> checkExpressionIsValue e >> return expr
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

checkNameIsValue :: Name -> ESSS ()
checkNameIsValue n = do
    s <- getNameKind n
    if s /= Value then throwE (SemanticError NotAValueError (show n))
    else return ()

checkNameIsPointer :: Name -> ESSS ()
checkNameIsPointer n = do
    s <- getNameKind n
    if s /= Pointer then throwE (SemanticError NotAPointerError (show n))
    else return ()

checkNameIsFunction :: Name -> ESSS ()
checkNameIsFunction n = do
    st <- get
    let s = nameToString n
    let info = unsafeGetSymbolInfo s st 
    case info of
        (VarInfo _ _ _) -> throwE (SemanticError NotAFunctionError (show n))
        (FuncInfo _ _) -> return ()

checkExpressionIsValue :: Expression -> ESSS ()
checkExpressionIsValue expr = do
    s <- getExpressionKind expr
    if s /= Value
        then throwE (SemanticError NotAValueError $ show expr)
    else return ()

checkExpressionIsPointer :: Expression -> ESSS ()
checkExpressionIsPointer expr = do
    s <- getExpressionKind expr
    if s /= Pointer
        then throwE (SemanticError NotAPointerError (show expr))
    else return ()

checkAssignment :: Name -> Expression -> ESSS ()
checkAssignment name expr = do
    s1 <- getNameKind name
    s2 <- getExpressionKind expr
    if s1 == Value && s2 /= Value
        then throwE (SemanticError NotAValueError (show expr))
    else if s1 == Pointer && s2 /= Pointer
        then throwE (SemanticError NotAPointerError (show expr))
    else
        return ()

checkArgument :: Expression -> SymbolInfo -> ESSS ()
checkArgument arg param = do
    s <- getExpressionKind arg
    if infoKind param == Value && s /= Value
        then throwE (SemanticError NotAValueError $ show arg)
    else if infoKind param == Pointer && s /= Pointer 
        then throwE (SemanticError NotAPointerError $ show arg)
    else return ()

checkArguments :: [Expression] -> Name -> ESSS ()
checkArguments args func = do
    st <- get
    let funcName = nameToString func
    let params = M.elems $ (infoParams ... unsafeGetSymbolInfo) funcName st 
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
getNameKind :: Name -> ESSS SymbolKind
getNameKind n = do
    st <- get
    let s = unsafeSymbolKind (nameToString n) st
    case n of
        (Name _) -> return s
        (NameSubscription _ _) -> 
            if s == Value then throwE (SemanticError NotAPointerError (show n))
            else return Value
        (NamePointer n) -> 
            if s == Value then throwE (SemanticError NotAPointerError (show n))
            else return Value
            
getExpressionKind :: Expression -> ESSS SymbolKind
getExpressionKind expr = do
    st <- get
    case expr of 
        (BinOp e1 _ e2) -> do
            s1 <- getExpressionKind e1
            s2 <- getExpressionKind e2
            if s1 == Value && s2 /= Value
                then throwE (SemanticError NotAValueError (show e1))
            else if s1 == Pointer && s2 /= Pointer
                then throwE (SemanticError NotAPointerError (show e2))
            else
                return s1
        (UnOp _ e) -> getExpressionKind e
        (Var name) -> getNameKind name
        (Address name) -> return Pointer
        _ -> return Value

-- API

run ::Program -> (Either SemanticError Program, SymbolTable)
run prog = (runState . runExceptT) (do
    st <- ExceptT $ return $ constructST prog
    put st
    entryPointExists prog >>= check)
    (zipper emptyST)

checkSemantics :: Program -> (Either SemanticError Program)
checkSemantics = fst . run

symbolTable :: Program -> SymbolTable
symbolTable = root . snd . run
