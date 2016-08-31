module TestParser (testParsing, scan_and_parse) where

import Test.Hspec
import Control.Exception (evaluate)

import Compiler (scan_and_parse)
import AST

testParsing = 
	describe "Parser" $ do
        it "Parses int declarations" $ do
            let ast = scan_and_parse "int a = 2; int b;"
            ast `shouldBe` [VarDeclaration IntType (Name "a") (Just $ Int 2), VarDeclaration IntType (Name "b") Nothing]
        it "Parses several declarations with too much semicolons" $ do
            let ast = scan_and_parse "int a = 2;;; int b = 3;; int c = 4;"
            ast `shouldBe` [VarDeclaration IntType (Name "a") (Just $ Int 2), VarDeclaration IntType (Name "b") (Just $ Int 3), VarDeclaration IntType (Name "c") (Just $ Int 4)]
        it "Parses declarations without semicolons and throws an exception" $ do
            let ast = scan_and_parse "int a = 2 int b = 3;"
            evaluate ast `shouldThrow` anyErrorCall
        it "Parses declarations with a statement and throws an exception" $ do
            let ast = scan_and_parse "int a = if (a < 5) a = 2;;"
            evaluate ast `shouldThrow` anyErrorCall
        it "Parses integer expressions" $ do
            let ast = scan_and_parse "int a = 2 + 3; int b = 2 + 3 * 4; int c = 2 / 4 + 3 * 2; int d = 8/2 + 1;"
            ast `shouldBe` [VarDeclaration IntType (Name "a") (Just $ BinOp (Int 2) Plus (Int 3)), VarDeclaration IntType (Name "b") (Just $ BinOp (Int 2) Plus $ BinOp (Int 3) Times (Int 4)), VarDeclaration IntType (Name "c") (Just $ BinOp (BinOp (Int 2) Divide (Int 4)) Plus (BinOp (Int 3) Times (Int 2))), VarDeclaration IntType (Name "d") (Just $ BinOp (BinOp (Int 8) Divide (Int 2)) Plus (Int 1))]
        it "Parses array expression" $ do
            let ast = scan_and_parse "int c = a[5];"
            ast `shouldBe` [VarDeclaration IntType (Name "c") (Just $ Var $ NameSubscription "a" [Int 5])]
        it "Parses unary operators" $ do
            let ast = scan_and_parse "int a = -5; int b = -7 + -5; int c = !5;"
            ast `shouldBe` [VarDeclaration IntType (Name "a") (Just $ UnOp Neg (Int 5)), VarDeclaration IntType (Name "b") (Just $ BinOp (UnOp Neg (Int 7)) Plus (UnOp Neg (Int 5))), VarDeclaration IntType (Name "c") (Just $ UnOp Not (Int 5))]
        it "Parses declaration of int with char expressions" $ do
            let ast = scan_and_parse "int a = 'c'; char c = 5;"
            ast `shouldBe` [VarDeclaration IntType (Name "a") (Just $ Char 'c'), VarDeclaration CharType (Name "c") (Just $ Int 5)]
        it "Parses integer and char expressions" $ do
            let ast = scan_and_parse "int c = 'c' + 'b'; int a = 2 + 'c'; char c = 'c' + 2; char c = 2 + 3;"
            ast `shouldBe` [VarDeclaration IntType (Name "c") (Just $ BinOp (Char 'c') Plus (Char 'b')), VarDeclaration IntType (Name "a") (Just $ BinOp (Int 2) Plus (Char 'c')), VarDeclaration CharType (Name "c") (Just $ BinOp (Char 'c') Plus (Int 2)), VarDeclaration CharType (Name "c") (Just $ BinOp (Int 2) Plus (Int 3))]
        it "Parses function call expression with no argument" $ do
            let ast = scan_and_parse "int c = a();"
            ast `shouldBe` [VarDeclaration IntType (Name "c") (Just $ Call (Name "a") [])]
        it "Parses function call expression with one argument" $ do
            let ast = scan_and_parse "int c = a(5);"
            ast `shouldBe` [VarDeclaration IntType (Name "c") (Just $ Call (Name "a") [Int 5])]
        it "Parses function call expression with several arguments" $ do
            let ast = scan_and_parse "int c = a(5, b);"
            ast `shouldBe` [VarDeclaration IntType (Name "c") (Just $ Call (Name "a") [Int 5, Var $ Name "b"])]
        it "Parses length expression" $ do
            let ast = scan_and_parse "int c = length a;"
            ast `shouldBe` [VarDeclaration IntType (Name "c") (Just $ Length $ Name "a")]
        it "Parses length expression with a statement and throws an exception" $ do
            let ast = scan_and_parse "int c = length (a = 2;)"
            evaluate ast `shouldThrow` anyErrorCall
        it "Parses parenthesis in expressions (for precedence)" $ do
            let ast = scan_and_parse "int a = (1 + 5) * 3;"
            ast `shouldBe`[VarDeclaration IntType (Name "a") (Just (BinOp (BinOp (Int 1) Plus (Int 5)) Times (Int 3)))]
        it "Parses array declarations" $ do
            let ast = scan_and_parse "int c[10]; char b[5+2];"
            ast `shouldBe` [VarDeclaration IntType (NameSubscription "c" [Int 10]) Nothing, VarDeclaration CharType (NameSubscription "b" [BinOp (Int 5) Plus (Int 2)]) Nothing]
        it "Parses function declarations with no args and empty block" $ do
            let ast = scan_and_parse "int tiny() { }"
            ast `shouldBe` [FuncDeclaration IntType (Name "tiny") [] (Block [] [])]
        it "Parses function declaration with one arg and empty block" $ do
            let ast = scan_and_parse "int tiny(int a) { }"
            ast `shouldBe` [FuncDeclaration IntType (Name "tiny") [Parameter IntType (Name "a")] (Block [] [])]
        it "Parses function declaration with two args and empty block" $ do
            let ast = scan_and_parse "int tiny(int a, char b) { }"
            ast `shouldBe` [FuncDeclaration IntType (Name "tiny") [Parameter IntType (Name "a"), Parameter CharType (Name "b")] (Block [] [])]
        it "Parses function declaration with no args and block with too much semicolons" $ do
            let ast = scan_and_parse "int tiny() { ;;; }"
            ast `shouldBe` [FuncDeclaration IntType (Name "tiny") [] (Block [] [])]
        it "Parses function declarations with declarations" $ do
            let ast = scan_and_parse "int tiny(int a) { int b = 2; }"
            ast `shouldBe` [FuncDeclaration IntType (Name "tiny") [Parameter IntType (Name "a")] (Block [VarDeclaration IntType (Name "b") (Just $ Int 2)] [])]
        it "Parses function declaration with declarations and too much semicolons" $ do
            let ast = scan_and_parse "int tiny() { ;; int b = 2;; int c = 3;;;}"
            ast `shouldBe` [FuncDeclaration IntType (Name "tiny") [] (Block [VarDeclaration IntType (Name "b") (Just $ Int 2), VarDeclaration IntType (Name "c") (Just $ Int 3)] [])]
        it "Parses function declaration with one statement" $ do
            let ast = scan_and_parse "int tiny() { b = 3; }"
            ast `shouldBe` [FuncDeclaration IntType (Name "tiny") [] (Block [] [Assignment (Name "b") (Int 3)])]
        it "Parses function declaration with several statements" $ do
            let ast = scan_and_parse "int tiny() { b = 3; c = 4; }"
            ast `shouldBe` [FuncDeclaration IntType (Name "tiny") [] (Block [] [Assignment (Name "b") (Int 3), Assignment (Name "c") (Int 4)])]
        it "Parses function declaration with several statements and too much semicolons" $ do
            let ast = scan_and_parse "int tiny() { ;;;b = 3;; c = 4;; }"
            ast `shouldBe` [FuncDeclaration IntType (Name "tiny") [] (Block [] [Assignment (Name "b") (Int 3), Assignment (Name "c") (Int 4)])]
        it "Parses function declaration with several declarations and statements" $ do
            let ast = scan_and_parse "int tiny() { int a = 4; int b; b = 3; c = 4; }"
            ast `shouldBe` [FuncDeclaration IntType (Name "tiny") [] (Block [VarDeclaration IntType (Name "a") (Just $ Int 4), VarDeclaration IntType (Name "b") Nothing] [Assignment (Name "b") (Int 3), Assignment (Name "c") (Int 4)])]
        it "Parses function declaration with statements before declarations and throws an exception" $ do
            let ast = scan_and_parse "int tiny() { a = 4; int b; b = 3; c = 4; }"
            evaluate ast `shouldThrow` anyErrorCall
        it "Parses array assignment" $ do
            let ast = scan_and_parse "int tiny() { c[5] = 3; }"
            ast `shouldBe` [FuncDeclaration IntType (Name "tiny") [] (Block [] [Assignment (NameSubscription "c" [Int 5]) (Int 3)])]
        it "Parses an if with one instruction " $ do
            let ast = scan_and_parse "int tiny() { if (a == 5) a = 3; }"
            ast `shouldBe` [FuncDeclaration IntType (Name "tiny") [] (Block [] [If (BinOp (Var $ Name "a") Equal (Int 5)) (Assignment (Name "a") (Int 3))])]
        it "Parses an if block" $ do
            let ast = scan_and_parse "int tiny() { if (a == 5) { a = 3; b = 'c';} }"
            ast `shouldBe` [FuncDeclaration IntType (Name "tiny") [] (Block [] [If (BinOp (Var $ Name "a") Equal (Int 5)) (Block [] [Assignment (Name "a") (Int 3), Assignment (Name "b") (Char 'c')])])]
        it "Parses an if else with 1/1 statement" $ do
            let ast = scan_and_parse "int tiny() { if (a == 5) a = 3; else a = 2; }"
            ast `shouldBe` [FuncDeclaration IntType (Name "tiny") [] (Block [] [IfElse (BinOp (Var $ Name "a") Equal (Int 5)) (Assignment (Name "a") (Int 3)) (Assignment (Name "a") (Int 2))])]
        it "Parses an if else with several / 1 statements" $ do
            let ast = scan_and_parse "int tiny() { if (a == 5) { a = 3; b = 'c';} else a = 3; }"
            ast `shouldBe` [FuncDeclaration IntType (Name "tiny") [] (Block [] [IfElse (BinOp (Var $ Name "a") Equal (Int 5)) (Block [] [Assignment (Name "a") (Int 3), Assignment (Name "b") (Char 'c')]) (Assignment (Name "a") (Int 3))])]
        it "Parses an if else with 1/several statements" $ do
            let ast = scan_and_parse "int tiny() { if (a == 5) a = 3; else { a = 2; b = 3; } }"
            ast `shouldBe` [FuncDeclaration IntType (Name "tiny") [] (Block [] [IfElse (BinOp (Var $ Name "a") Equal (Int 5)) (Assignment (Name "a") (Int 3)) (Block [] [Assignment (Name "a") (Int 2), Assignment (Name "b") (Int 3)])])]
        it "Parses an if else with several/several statements" $ do
            let ast = scan_and_parse "int tiny() { if (a == 5) { a = 3; b = 2; } else { a = 2; b = 3; } }"
            ast `shouldBe` [FuncDeclaration IntType (Name "tiny") [] (Block [] [IfElse (BinOp (Var $ Name "a") Equal (Int 5)) (Block [] [Assignment (Name "a") (Int 3), Assignment (Name "b") (Int 2)]) (Block [] [Assignment (Name "a") (Int 2), Assignment (Name "b") (Int 3)])])]
        it "Parses an if else statement with missing body and throws an exception" $ do
            let ast = scan_and_parse "int tiny() { if (a == 5) else a = 2; }"
            evaluate ast `shouldThrow` anyErrorCall
            let ast = scan_and_parse "int tiny() { if (a) a = 2; else"
            evaluate ast `shouldThrow` anyErrorCall
        it "Parses an if else if statement" $ do
            let ast = scan_and_parse "int tiny() { if (a == 5) a = 5; else if (a == 3) a = 3; }"
            ast `shouldBe` [FuncDeclaration IntType (Name "tiny") [] (Block [] [IfElse (BinOp (Var (Name "a")) Equal (Int 5)) (Assignment (Name "a") (Int 5)) (If (BinOp (Var (Name "a")) Equal (Int 3)) (Assignment (Name "a") (Int 3)))])]
        it "Parses a while with one instruction" $ do
            let ast = scan_and_parse "int tiny() { while (a == 5) a = 3; b = 2; }"
            ast `shouldBe` [FuncDeclaration IntType (Name "tiny") [] (Block [] [While (BinOp (Var $ Name "a") Equal (Int 5)) (Assignment (Name "a") (Int 3)), Assignment (Name "b") (Int 2)])]
        it "Parses a while block" $ do
            let ast = scan_and_parse "int tiny() { while (a == 5) { int b = 'c'; a = 4; } }"
            ast `shouldBe` [FuncDeclaration IntType (Name "tiny") [] (Block [] [While (BinOp (Var $ Name "a") Equal (Int 5)) (Block [VarDeclaration IntType (Name "b") (Just $ Char 'c')] [Assignment (Name "a") (Int 4)])])]
        it "Parses return statement" $ do
            let ast = scan_and_parse "int tiny() { return x + 5; }"
            ast `shouldBe` [ FuncDeclaration IntType (Name "tiny") [] (Block [] [Return $ BinOp (Var $ Name "x") Plus (Int 5)])]
        it "Parses function call without args" $ do
            let ast = scan_and_parse "int tiny() { lol(); }"
            ast `shouldBe` [FuncDeclaration IntType (Name "tiny") [] (Block [] [Expr $ Call (Name "lol") []])]
        it "Parses function call with one arg" $ do
            let ast = scan_and_parse "int tiny() { lol(x); }"
            ast `shouldBe` [FuncDeclaration IntType (Name "tiny") [] (Block [] [Expr $ Call (Name "lol") [Var $ Name "x"]])]
        it "Parses function call with several args" $ do
            let ast = scan_and_parse "int tiny() { lol(2, x); }"
            ast `shouldBe` [FuncDeclaration IntType (Name "tiny") [] (Block [] [Expr $ Call (Name "lol") [Int 2, Var $ Name "x"]])]
        it "Parses read and write statements" $ do
            let ast = scan_and_parse "int tiny() { read x; write 2; write x; }"
            ast `shouldBe` [FuncDeclaration IntType (Name "tiny") [] (Block [] [Read (Name "x"), Write (Int 2), Write (Var $ Name "x")])]
        it "Throws an parse error" $ do
            let ast = scan_and_parse "int tiny() { read x }"
            evaluate ast `shouldThrow` errorCall "Parse error at line 1 column 21 : \"}\""
   