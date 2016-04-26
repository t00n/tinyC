import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate, ErrorCall)
import System.IO


import Scanner
import Parser
import Semantics
import TACGenerator
import MonadNames

scan_and_parse = parse . alexScanTokens

scan_parse_check xs = let ast = scan_and_parse xs 
                          check = checkSemantics ast in
    if check == Right () then ast
    else error $ show check

testTokens s r = do
    it ("Tokenizes " ++ s) $ do
        s <- readFile ("test/fixtures/" ++ s)
        (alexScanTokens s) `shouldBe` r

testTokensThrow s e = do
    it ("Tokenizes " ++ s) $ do
        s <- readFile ("test/fixtures/" ++ s)
        evaluate (alexScanTokens s) `shouldThrow` e

main :: IO ()
main = hspec $ do
    describe "Tokenizer" $ do
        testTokens "trivial.c" [INT,NAME "a",SEMICOLON,CHAR,NAME "c",SEMICOLON,INT,NAME "a",ASSIGN,NUMBER 2,SEMICOLON,CHAR,NAME "c",ASSIGN,QCHAR 'b',SEMICOLON,INT,NAME "as",LBRACK,NUMBER 10,RBRACK,SEMICOLON,IF,LPAR,NAME "a",LESS,NUMBER 5,RPAR,LBRACE,RBRACE,IF,LPAR,NAME "a",GREATER,NUMBER 5,RPAR,LBRACE,RBRACE,ELSE,IF,LPAR,NAME "a",EQUAL,NUMBER 5,RPAR,LBRACE,RBRACE,ELSE,IF,LPAR,NAME "a",NEQUAL,NUMBER 5,RPAR,LBRACE,RBRACE,ELSE,LBRACE,RBRACE,NAME "a",ASSIGN,NUMBER 5,TIMES,NUMBER 4,SEMICOLON,NAME "a",ASSIGN,NUMBER 20,DIVIDE,NUMBER 2,SEMICOLON,NAME "a",ASSIGN,NUMBER 5,PLUS,NUMBER 6,SEMICOLON,NAME "a",ASSIGN,NUMBER 8,MINUS,NUMBER 5,SEMICOLON,NAME "a",ASSIGN,NOT,NUMBER 1,SEMICOLON,NAME "a",ASSIGN,MINUS,NUMBER 5,SEMICOLON,NAME "as",LBRACK,NUMBER 5,RBRACK,ASSIGN,NUMBER 5,SEMICOLON,WRITE,LPAR,QSTRING "lololol",RPAR,SEMICOLON,READ,LPAR,RPAR,SEMICOLON,LENGTH,LPAR,NAME "as",RPAR,SEMICOLON,INT,NAME "i",ASSIGN,NUMBER 0,SEMICOLON,WHILE,LPAR,NAME "i",LESS,NUMBER 10,RPAR,LBRACE,WRITE,LPAR,NAME "i",RPAR,SEMICOLON,RBRACE,INT,NAME "caca",LPAR,RPAR,LBRACE,RETURN,NUMBER 42,SEMICOLON,RBRACE,CHAR,NAME "lol",LPAR,INT,NAME "n",RPAR,LBRACE,RETURN,NAME "n",SEMICOLON,RBRACE,INT,NAME "lol2",LPAR,INT,NAME "a",COMMA,INT,NAME "b",COMMA,CHAR,NAME "c",RPAR,LBRACE,RETURN,QSTRING "haha",SEMICOLON,RBRACE]
        testTokens "fibonacci.c" [INT,NAME "fibonacci",LPAR,INT,NAME "n",RPAR,LBRACE,IF,LPAR,NAME "n",LESS,NUMBER 0,RPAR,LBRACE,RETURN,MINUS,NUMBER 1,SEMICOLON,RBRACE,ELSE,IF,LPAR,NAME "n",LESS,NUMBER 1,RPAR,LBRACE,RETURN,NUMBER 0,SEMICOLON,RBRACE,ELSE,IF,LPAR,NAME "n",LESS,NUMBER 2,RPAR,LBRACE,RETURN,NUMBER 1,SEMICOLON,RBRACE,ELSE,LBRACE,RETURN,NAME "fibonacci",LPAR,NAME "n",MINUS,NUMBER 1,RPAR,PLUS,NAME "fibonacci",LPAR,NAME "n",MINUS,NUMBER 2,RPAR,SEMICOLON,RBRACE,RBRACE,INT,NAME "tiny",LPAR,RPAR,LBRACE,INT,NAME "i",ASSIGN,NUMBER 0,SEMICOLON,WHILE,LPAR,NAME "i",LESS,NUMBER 10,RPAR,LBRACE,WRITE,NAME "fibonacci",LPAR,NAME "i",RPAR,SEMICOLON,NAME "i",ASSIGN,NAME "i",PLUS,NUMBER 1,SEMICOLON,RBRACE,RBRACE]
        testTokensThrow "wrong.c" anyErrorCall
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
            ast `shouldBe` [VarDeclaration IntType (Name "c") (Just $ Var $ NameSubscription "a" (Int 5))]
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
            ast `shouldBe` [VarDeclaration IntType (NameSubscription "c" (Int 10)) Nothing, VarDeclaration CharType (NameSubscription "b" (BinOp (Int 5) Plus (Int 2))) Nothing]
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
            ast `shouldBe` [FuncDeclaration IntType (Name "tiny") [] (Block [] [Assignment (NameSubscription "c" (Int 5)) (Int 3)])]
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
    describe "Semantics" $ do
        it "Checks that variables with same name are declared only once on a certain scope level" $ do
            let ast = scan_and_parse "int tiny() { int a; int a; }"
            checkSemantics ast `shouldBe` Left (SemanticError NameExistsError "a")
        it "Checks that variables with same name can be declared more than once a different scope level with a warning" $ do
            let ast = scan_and_parse "int a; int tiny() { int a; }"
            checkSemantics ast `shouldBe` Left (SemanticError NameExistsWarning "a")
        it "Checks that variables are declared before use in assignment" $ do
            let ast = scan_and_parse "int tiny() { a = 5; }"
            checkSemantics ast `shouldBe` Left (SemanticError NotDeclaredError "a")
            let ast = scan_and_parse "int a = 5; int tiny() { a = 3; }"
            checkSemantics ast `shouldBe` Right ()
        it "Checks that variables are declared before use in if" $ do
            let ast = scan_and_parse "int tiny() { if (a) a = 5; }"
            checkSemantics ast `shouldBe` Left (SemanticError NotDeclaredError "a")
            let ast = scan_and_parse "int a = 5; int tiny() { int c = 3; if (a) a = 3; }"
            checkSemantics ast `shouldBe` Right ()
        it "Checks that variables are declared before use in while" $ do
            let ast = scan_and_parse "int tiny() { while (a) a = 5; }"
            checkSemantics ast `shouldBe` Left (SemanticError NotDeclaredError "a")
            let ast = scan_and_parse "int a = 5; int tiny() { while (a) a = 5; }"
            checkSemantics ast `shouldBe` Right ()
        it "Checks that variables are declared before use in return" $ do
            let ast = scan_and_parse "int tiny() { return a; }"
            checkSemantics ast `shouldBe` Left (SemanticError NotDeclaredError "a")
            let ast = scan_and_parse "int a = 5; int tiny() { return a; }"
            checkSemantics ast `shouldBe` Right ()
        it "Checks that variables are declared and scalar before use in IO" $ do
            let ast = scan_and_parse "int tiny() { write a; }"
            checkSemantics ast `shouldBe` Left (SemanticError NotDeclaredError "a")
            let ast = scan_and_parse "int a = 5; int tiny() { read a; }"
            checkSemantics ast `shouldBe` Right ()
            let ast = scan_and_parse "int a = 5; int tiny() { write a; }"
            checkSemantics ast `shouldBe` Right ()
            let ast = scan_and_parse "int a[5]; int tiny() { write a; }"
            checkSemantics ast `shouldBe`  Left (SemanticError {errorType = NotAScalarError, errorVariable = "Var (Name \"a\")"})
            let ast = scan_and_parse "int a[5]; int tiny() { read a; }"
            checkSemantics ast `shouldBe`  Left (SemanticError {errorType = NotAScalarError, errorVariable = "a"})
            let ast = scan_and_parse "int a; int tiny() { read a[5]; }"
            checkSemantics ast `shouldBe` Left (SemanticError {errorType = NotAnArrayError, errorVariable = "NameSubscription \"a\" (Int 5)"})
            let ast = scan_and_parse "int a[5]; int tiny() { read a[2]; }"
            checkSemantics ast `shouldBe` Right ()
        it "Checks that variables are declared before use in expression" $ do
            let ast = scan_and_parse "int tiny() { a; }"
            checkSemantics ast `shouldBe` Left (SemanticError NotDeclaredError "a")
            let ast = scan_and_parse "int a = 5; int tiny() { a; }"
            checkSemantics ast `shouldBe` Right ()
        it "Checks that variables are declared before use in binary operations" $ do
            let ast = scan_and_parse "int tiny() { a + 5; }"
            checkSemantics ast `shouldBe` Left (SemanticError NotDeclaredError "a")
            let ast = scan_and_parse "int a = 5; int tiny() { a + 5; }"
            checkSemantics ast `shouldBe` Right ()
            let ast = scan_and_parse "int tiny() { 5 + a; }"
            checkSemantics ast `shouldBe` Left (SemanticError NotDeclaredError "a")
            let ast = scan_and_parse "int a = 5; int tiny() { 5 + a; }"
            checkSemantics ast `shouldBe` Right ()
        it "Checks that variables are declared before use in unary operations" $ do
            let ast = scan_and_parse "int tiny() { -a; }"
            checkSemantics ast `shouldBe` Left (SemanticError NotDeclaredError "a")
            let ast = scan_and_parse "int a = 5; int tiny() { -a; }"
            checkSemantics ast `shouldBe` Right ()
        it "Checks that variables are declared before use in function calls and that the variable is a function" $ do
            let ast = scan_and_parse "int tiny() { a(); }"
            checkSemantics ast `shouldBe` Left (SemanticError NotDeclaredError "a")
            let ast = scan_and_parse "int a = 5; int tiny() { a(); }"
            checkSemantics ast `shouldBe` Left (SemanticError NotAFunctionError "a")
            let ast = scan_and_parse "int tiny() { tiny(); }"
            checkSemantics ast `shouldBe` Right ()
        it "Checks that parameters of a function are declared and variables in the scope" $ do
            let ast = scan_and_parse "int tiny(int a, char c) { a + 5; }"
            checkSemantics ast `shouldBe` Right ()
            let ast = scan_and_parse "int tiny(int a) { int a; }"
            checkSemantics ast `shouldBe` Left (SemanticError {errorType = NameExistsError, errorVariable = "a"})
        it "Checks the arguments of a function call" $ do
            let ast = scan_and_parse "int a = 5; int tiny(int a, int b) { tiny(a, 5); }"
            checkSemantics ast `shouldBe` Left (SemanticError {errorType = NameExistsWarning, errorVariable = "a"})
            let ast = scan_and_parse "int tiny(int a, int b) { tiny(a, c); }"
            checkSemantics ast `shouldBe` Left (SemanticError {errorType = NotDeclaredError, errorVariable = "c"})
            let ast = scan_and_parse "int a[5]; int tiny(int b) { tiny(a); }"
            checkSemantics ast `shouldBe` Left (SemanticError {errorType = NotAScalarError, errorVariable = "Var (Name \"a\")"})
            let ast = scan_and_parse "int a; int tiny(int b[5]) { tiny(a); }"
            checkSemantics ast `shouldBe` Left (SemanticError {errorType = NotAnArrayError, errorVariable = "Var (Name \"a\")"})
        it "Checks that variables are declared before use in a length expression and that the variable is an array" $ do
            let ast = scan_and_parse "int tiny() { length a; }"
            checkSemantics ast `shouldBe` Left (SemanticError NotDeclaredError "a")
            let ast = scan_and_parse "int a; int tiny() { length a; }"
            checkSemantics ast `shouldBe` Left (SemanticError NotAnArrayError "a")
            let ast = scan_and_parse "int a[5]; int tiny() { length a; }"
            checkSemantics ast `shouldBe` Right ()
        it "Checks that arrays are declared with constant/literals size" $ do
            let ast = scan_and_parse "int a = 5; int b[a];"
            checkSemantics ast `shouldBe` Left (SemanticError {errorType = NotConstantSizeArrayError, errorVariable = "NameSubscription \"b\" (Var (Name \"a\"))"})
        it "Checks that only scalar expressions are used in binary and unary operations" $ do
            let ast = scan_and_parse "int a[5]; int tiny() { a + 5; }"
            checkSemantics ast `shouldBe` Left (SemanticError NotAScalarError "Var (Name \"a\")")
            let ast = scan_and_parse "int a[5]; int tiny() { a[2] + 5; }"
            checkSemantics ast `shouldBe` Right ()
            let ast = scan_and_parse "int a = 5; int tiny() { a + 5; }"
            checkSemantics ast `shouldBe` Right ()
        it "Checks that name subscriptions are used with an array" $ do
            let ast = scan_and_parse "int a = 5; int tiny() { a[5] + 5; }"
            checkSemantics ast `shouldBe` Left (SemanticError {errorType = NotAnArrayError, errorVariable = "a"})
        it "Checks that names are subscribed with scalar expressions" $ do
            let ast = scan_and_parse "int a[5]; int b[6]; int tiny() { a[b]; }"
            checkSemantics ast `shouldBe` Left (SemanticError {errorType = NotAScalarError, errorVariable = "Var (Name \"b\")"})
            let ast = scan_and_parse "int a[5]; int b[6]; int tiny() { a[b[3]]; }"
            checkSemantics ast `shouldBe` Right ()
        it "Checks that assignments have the same scalarity" $ do
            let ast = scan_and_parse "int a; int b; int tiny() { a = b; }"
            checkSemantics ast `shouldBe` Right ()
            let ast = scan_and_parse "int a; int b[5]; int tiny() { a = b; }"
            checkSemantics ast `shouldBe` Left (SemanticError {errorType = NotSameScalarityError, errorVariable = "Name \"a\" Var (Name \"b\")"})
            let ast = scan_and_parse "int a[5]; int b; int tiny() { a = b; }"
            checkSemantics ast `shouldBe` Left (SemanticError {errorType = NotSameScalarityError, errorVariable = "Name \"a\" Var (Name \"b\")"})
            let ast = scan_and_parse "int a; int b[5]; int tiny() { a = b[2]; }"
            checkSemantics ast `shouldBe` Right ()
            let ast = scan_and_parse "int a; int b[5]; int tiny() { b[2] = a; }"
            checkSemantics ast `shouldBe` Right ()
            let ast = scan_and_parse "int a[2]; int b[5]; int tiny() { a = b; }"
            checkSemantics ast `shouldBe` Right ()
        it "Checks that return statement have scalar expression" $ do
            let ast = scan_and_parse "int tiny() { return 4; }"
            checkSemantics ast `shouldBe` Right ()
            let ast = scan_and_parse "int b[5]; int tiny() { return b; }"
            checkSemantics ast `shouldBe` Left (SemanticError {errorType = NotAScalarError, errorVariable = "Var (Name \"b\")"})
        it "Checks that if and while statements have scalar expressions" $ do
            let ast = scan_and_parse "int tiny() { while (5) {} if (5) {} }"
            checkSemantics ast `shouldBe` Right ()
            let ast = scan_and_parse "int a[5]; int tiny() { while (a) {} }"
            checkSemantics ast `shouldBe` Left (SemanticError {errorType = NotAScalarError, errorVariable = "Var (Name \"a\")"})
            let ast = scan_and_parse "int a[5]; int tiny() { if (a) {} }"
            checkSemantics ast `shouldBe` Left (SemanticError {errorType = NotAScalarError, errorVariable = "Var (Name \"a\")"})
            let ast = scan_and_parse "int a[5]; int tiny() { if (a) {} else if (a) {} }"
            checkSemantics ast `shouldBe` Left (SemanticError {errorType = NotAScalarError, errorVariable = "Var (Name \"a\")"})
    describe "The generation of three-address-code" $ do
        it "Generates a few declarations" $ do
            let ast = scan_parse_check "int a; int b;"
            generateTAC ast `shouldBe` [TACDeclaration (TACVar "a"),TACDeclaration (TACVar "b")]
        it "Generates declarations with complex binary expressions" $ do
            let ast = scan_parse_check "int a = 5; int b = (a+5)/(a-2);"
            generateTAC ast `shouldBe` [TACDeclaration (TACVar "a"),TACCopy "a" (TACInt 5),TACDeclaration (TACVar "b"),TACBinary "t1" (TACVar "a") TACPlus (TACInt 5),TACBinary "t2" (TACVar "a") TACMinus (TACInt 2),TACBinary "t3" (TACVar "t1") TACDivide (TACVar "t2"),TACCopy "b" (TACVar "t3")]
        it "Generates function declarations" $ do
            let ast = scan_parse_check "int tiny() {}"
            generateTAC ast `shouldBe` [TACLabel "tiny",TACReturn (TACInt 0)]
            let ast = scan_parse_check "int tiny(int a, int b) { int c = 4; }"
            generateTAC ast `shouldBe` [TACLabel "tiny",TACParam "a",TACParam "b",TACDeclaration (TACVar "c"),TACCopy "c" (TACInt 4),TACReturn (TACInt 0)]
        it "Generates declarations with complex unary expressions" $ do
            let ast = scan_parse_check "int a = 5; int b = -(a - 5);"
            generateTAC ast `shouldBe` [TACDeclaration (TACVar "a"),TACCopy "a" (TACInt 5),TACDeclaration (TACVar "b"),TACBinary "t1" (TACVar "a") TACMinus (TACInt 5),TACUnary "t2" TACNeg (TACVar "t1"),TACCopy "b" (TACVar "t2")]
        it "Generates function calls" $ do
            let ast = scan_parse_check "int tiny(int a, int b) { int c = 1; tiny(c, 2); }"
            generateTAC ast `shouldBe` [TACLabel "tiny",TACParam "a",TACParam "b",TACDeclaration (TACVar "c"),TACCopy "c" (TACInt 1),TACCall "tiny" [TACVar "c",TACInt 2,TACVar "t1"],TACReturn (TACInt 0)]
            let ast = scan_parse_check "int tiny(int a, int b) { int c = tiny(2 + 3, 1); }"
            generateTAC ast `shouldBe` [TACLabel "tiny",TACParam "a",TACParam "b",TACDeclaration (TACVar "c"),TACBinary "t1" (TACInt 2) TACPlus (TACInt 3),TACCall "tiny" [TACVar "t1",TACInt 1,TACVar "t2"],TACCopy "c" (TACVar "t2"),TACReturn (TACInt 0)]
        it "Generates assignments" $ do
            let ast = scan_parse_check "int tiny() { int a; a = (a + 5) * 3; }"
            generateTAC ast `shouldBe` [TACLabel "tiny",TACDeclaration (TACVar "a"),TACBinary "t1" (TACVar "a") TACPlus (TACInt 5),TACBinary "t2" (TACVar "t1") TACTimes (TACInt 3),TACCopy "a" (TACVar "t2"),TACReturn (TACInt 0)]
            let ast = scan_parse_check "int a[5]; int b = a[2];"
            generateTAC ast `shouldBe` [TACDeclaration (TACArray "a" (TACInt 5)),TACDeclaration (TACVar "b"),TACArrayAccess "t1" (TACArray "a" (TACInt 2)),TACCopy "b" (TACVar "t1")]
            let ast = scan_parse_check "int a[5]; int tiny() { a[2] = 5; }"
            generateTAC ast `shouldBe` [TACDeclaration (TACArray "a" (TACInt 5)),TACLabel "tiny",TACArrayModif (TACArray "a" (TACInt 2)) (TACInt 5),TACReturn (TACInt 0)]
            let ast = scan_parse_check "int a[5]; int b[5]; int tiny() { a[2] = b[3]; }"
            generateTAC ast `shouldBe` [TACDeclaration (TACArray "a" (TACInt 5)),TACDeclaration (TACArray "b" (TACInt 5)),TACLabel "tiny",TACArrayAccess "t1" (TACArray "b" (TACInt 3)),TACArrayModif (TACArray "a" (TACInt 2)) (TACVar "t1"),TACReturn (TACInt 0)]
        it "Generates if" $ do
            let ast = scan_parse_check "int tiny() { if (1 > 2) { int a = 5; a = 3; } }"
            generateTAC ast `shouldBe` [TACLabel "tiny",TACBinary "t1" (TACInt 1) TACGreater (TACInt 2),TACIf (TACVar "t1") "l1",TACGoto "l2",TACLabel "l1",TACDeclaration (TACVar "a"),TACCopy "a" (TACInt 5),TACCopy "a" (TACInt 3),TACLabel "l2",TACReturn (TACInt 0)]
        it "Generates if else" $ do
            let ast = scan_parse_check "int tiny() { if (1 > 2) { int a = 5; } else { int b = 5; } }"
            generateTAC ast `shouldBe` [TACLabel "tiny",TACBinary "t1" (TACInt 1) TACGreater (TACInt 2),TACIf (TACVar "t1") "l1",TACGoto "l2",TACLabel "l1",TACDeclaration (TACVar "a"),TACCopy "a" (TACInt 5),TACGoto "l3",TACLabel "l2",TACDeclaration (TACVar "b"),TACCopy "b" (TACInt 5),TACLabel "l3",TACReturn (TACInt 0)]
        it "Generates several if else" $ do
            let ast = scan_parse_check "int tiny() { if (1 > 2) { int a = 1; } else if (2 > 3) { int b = 2; } else { int c = 3; } }"
            generateTAC ast `shouldBe` [TACLabel "tiny",TACBinary "t1" (TACInt 1) TACGreater (TACInt 2),TACIf (TACVar "t1") "l1",TACGoto "l2",TACLabel "l1",TACDeclaration (TACVar "a"),TACCopy "a" (TACInt 1),TACGoto "l3",TACLabel "l2",TACBinary "t2" (TACInt 2) TACGreater (TACInt 3),TACIf (TACVar "t2") "l4",TACGoto "l5",TACLabel "l4",TACDeclaration (TACVar "b"),TACCopy "b" (TACInt 2),TACGoto "l6",TACLabel "l5",TACDeclaration (TACVar "c"),TACCopy "c" (TACInt 3),TACLabel "l6",TACLabel "l3",TACReturn (TACInt 0)]
        it "Generates a while" $ do
            let ast = scan_parse_check "int tiny() { int a = 2; while ( a > 1) { a = a - 1; } }"
            generateTAC ast `shouldBe` [TACLabel "tiny",TACDeclaration (TACVar "a"),TACCopy "a" (TACInt 2),TACLabel "l1",TACBinary "t1" (TACVar "a") TACGreater (TACInt 1),TACIf (TACVar "t1") "l3",TACGoto "l2",TACLabel "l3",TACBinary "t2" (TACVar "a") TACMinus (TACInt 1),TACCopy "a" (TACVar "t2"),TACGoto "l1",TACLabel "l2",TACReturn (TACInt 0)]
        it "Generates a return" $ do
            let ast = scan_parse_check "int tiny() { int a = 2; return a; }"
            generateTAC ast `shouldBe` [TACLabel "tiny",TACDeclaration (TACVar "a"),TACCopy "a" (TACInt 2),TACReturn (TACVar "a"),TACReturn (TACInt 0)]
            let ast = scan_parse_check "int tiny() { return 3 + 4 / 5; }"
            generateTAC ast `shouldBe` [TACLabel "tiny",TACBinary "t1" (TACInt 4) TACDivide (TACInt 5),TACBinary "t2" (TACInt 3) TACPlus (TACVar "t1"),TACReturn (TACVar "t2"),TACReturn (TACInt 0)]
        it "Generates a write" $ do
            let ast = scan_parse_check "int tiny() { int a = 2; write a; }"
            generateTAC ast `shouldBe` [TACLabel "tiny",TACDeclaration (TACVar "a"),TACCopy "a" (TACInt 2),TACWrite (TACVar "a"),TACReturn (TACInt 0)]
            let ast = scan_parse_check "int tiny() { int a[5]; write a[2]; }"
            generateTAC ast `shouldBe` [TACLabel "tiny",TACDeclaration (TACArray "a" (TACInt 5)),TACArrayAccess "t1" (TACArray "a" (TACInt 2)),TACWrite (TACVar "t1"),TACReturn (TACInt 0)]
        it "Generates reads" $ do
            let ast = scan_parse_check "int tiny() { int a; read a; }"
            generateTAC ast `shouldBe` [TACLabel "tiny",TACDeclaration (TACVar "a"),TACRead (TACVar "a"),TACReturn (TACInt 0)]
            let ast = scan_parse_check "int tiny() { int a[5]; read a[2]; }"
            generateTAC ast `shouldBe` [TACLabel "tiny",TACDeclaration (TACArray "a" (TACInt 5)),TACRead (TACArray "a" (TACInt 2)),TACReturn (TACInt 0)]
    describe "Do the name generator works ????" $ do
        it "Tests everything" $ do
            evalNames (do { s1 <- popVariable; s2 <- nextVariable; l1 <- nextLabel; return [s1, s2, l1] }) ["t" ++ show i | i <- [1..]] ["l" ++ show i | i <- [1..]] `shouldBe` ["t1", "t2", "l1"]
