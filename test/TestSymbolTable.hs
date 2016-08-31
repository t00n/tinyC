module TestSymbolTable (testSymbolTable) where

import Test.Hspec
import qualified Data.Tree as T
import qualified Data.Map as M

import TestParser (scan_and_parse)

import AST
import SymbolTable
import SemanticError

testSymbolTable = describe "Symbol table construction" $ do
        it "Constructs a one-level symbol table" $ do
            let ast = scan_and_parse "char a; int b = 5; int c[5]; int * p;"
            fmap root (constructST ast) `shouldBe` Right (zipper (T.Node (M.fromList [("a",VarInfo CharType Value (VarSize 1)),("b",VarInfo IntType Value (VarSize 1)),("c",VarInfo IntType Array (ArraySize [5])),("p", VarInfo IntType Pointer (VarSize 1))]) []))
        it "Fails to construct a symbol table when a name exists" $ do
            let ast = scan_and_parse "char a; int a[5];"
            fmap root (constructST ast) `shouldBe` Left (SemanticError {errorType = NameExistsError, errorVariable = "a"})
            let ast = scan_and_parse "int a; char a() {}"
            fmap root (constructST ast) `shouldBe` Left (SemanticError {errorType = NameExistsError, errorVariable = "a"})
        it "Constructs a two-level symbol table" $ do
            let ast = scan_and_parse "char a; int b = 5; int c[5]; char f() {} int g(int a) {}"
            fmap root (constructST ast) `shouldBe` Right (zipper (T.Node (M.fromList [("a",VarInfo CharType Value (VarSize 1)),("b",VarInfo IntType Value (VarSize 1)),("c",VarInfo IntType Array (ArraySize [5])),("f",FuncInfo CharType []),("g",FuncInfo IntType ([("a", VarInfo IntType Value (VarSize 1))]))]) [T.Node (M.fromList []) [],T.Node (M.fromList [("a",VarInfo IntType Value (VarSize 1))]) []]))