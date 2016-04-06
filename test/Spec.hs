import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate, ErrorCall)
import System.IO


import Scanner
import Parser
import Semantics

scan_and_parse = parse . alexScanTokens

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
        testTokens "fibonacci.c" [INT,NAME "fibonacci",LPAR,INT,NAME "n",RPAR,LBRACE,IF,LPAR,NAME "n",LESS,NUMBER 0,RPAR,LBRACE,RETURN,MINUS,NUMBER 1,SEMICOLON,RBRACE,ELSE,IF,LPAR,NAME "n",LESS,NUMBER 1,RPAR,LBRACE,RETURN,NUMBER 0,SEMICOLON,RBRACE,ELSE,IF,LPAR,NAME "n",LESS,NUMBER 2,RPAR,LBRACE,RETURN,NUMBER 1,SEMICOLON,RBRACE,ELSE,LBRACE,RETURN,NAME "fibonacci",LPAR,NAME "n",MINUS,NUMBER 1,RPAR,PLUS,NAME "fibonacci",LPAR,NAME "n",MINUS,NUMBER 2,RPAR,SEMICOLON,RBRACE,RBRACE,INT,NAME "main",LPAR,INT,NAME "argc",COMMA,CHAR,TIMES,TIMES,NAME "argv",RPAR,LBRACE,INT,NAME "i",ASSIGN,NUMBER 0,SEMICOLON,WHILE,LPAR,NAME "i",LESS,NUMBER 10,RPAR,LBRACE,WRITE,LPAR,NAME "fibonacci",LPAR,NAME "i",RPAR,RPAR,SEMICOLON,NAME "i",ASSIGN,NAME "i",PLUS,NUMBER 1,SEMICOLON,RBRACE,RBRACE]
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
            let ast2 = scan_and_parse "int tiny() { if (a) a = 2; else"
            evaluate ast2 `shouldThrow` anyErrorCall
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
            let ast2 = scan_and_parse "int a = 5; int tiny() { a = 3; }"
            checkSemantics ast2 `shouldBe` Right ()
        it "Checks that variables are declared before use in if" $ do
            let ast = scan_and_parse "int tiny() { if (a) a = 5; }"
            checkSemantics ast `shouldBe` Left (SemanticError NotDeclaredError "a")
            let ast2 = scan_and_parse "int a = 5; int tiny() { int c = 3; if (a) a = 3; }"
            checkSemantics ast2 `shouldBe` Right ()
        it "Checks that variables are declared before use in while" $ do
            let ast = scan_and_parse "int tiny() { while (a) a = 5; }"
            checkSemantics ast `shouldBe` Left (SemanticError NotDeclaredError "a")
            let ast2 = scan_and_parse "int a = 5; int tiny() { while (a) a = 5; }"
            checkSemantics ast2 `shouldBe` Right ()
        it "Checks that variables are declared before use in return" $ do
            let ast = scan_and_parse "int tiny() { return a; }"
            checkSemantics ast `shouldBe` Left (SemanticError NotDeclaredError "a")
            let ast2 = scan_and_parse "int a = 5; int tiny() { return a; }"
            checkSemantics ast2 `shouldBe` Right ()
        it "Checks that variables are declared before use in IO" $ do
            let ast = scan_and_parse "int tiny() { write a; }"
            checkSemantics ast `shouldBe` Left (SemanticError NotDeclaredError "a")
            let ast2 = scan_and_parse "int a = 5; int tiny() { read a; }"
            checkSemantics ast2 `shouldBe` Right ()
        it "Checks that variables are declared before use in expression" $ do
            let ast = scan_and_parse "int tiny() { a; }"
            checkSemantics ast `shouldBe` Left (SemanticError NotDeclaredError "a")
            let ast2 = scan_and_parse "int a = 5; int tiny() { a; }"
            checkSemantics ast2 `shouldBe` Right ()
        it "Checks that variables are declared before use in binary operations" $ do
            let ast = scan_and_parse "int tiny() { a + 5; }"
            checkSemantics ast `shouldBe` Left (SemanticError NotDeclaredError "a")
            let ast2 = scan_and_parse "int a = 5; int tiny() { a + 5; }"
            checkSemantics ast2 `shouldBe` Right ()
            let ast3 = scan_and_parse "int tiny() { 5 + a; }"
            checkSemantics ast3 `shouldBe` Left (SemanticError NotDeclaredError "a")
            let ast4 = scan_and_parse "int a = 5; int tiny() { 5 + a; }"
            checkSemantics ast4 `shouldBe` Right ()
        it "Checks that variables are declared before use in unary operations" $ do
            let ast = scan_and_parse "int tiny() { -a; }"
            checkSemantics ast `shouldBe` Left (SemanticError NotDeclaredError "a")
            let ast2 = scan_and_parse "int a = 5; int tiny() { -a; }"
            checkSemantics ast2 `shouldBe` Right ()
        it "Checks that variables are declared before use in function calls and that the variable is a function" $ do
            let ast = scan_and_parse "int tiny() { a(); }"
            checkSemantics ast `shouldBe` Left (SemanticError NotDeclaredError "a")
            let ast2 = scan_and_parse "int a = 5; int tiny() { a(); }"
            checkSemantics ast2 `shouldBe` Left (SemanticError NotAFunctionError "a")
            let ast3 = scan_and_parse "int tiny() { tiny(); }"
            checkSemantics ast3 `shouldBe` Right ()
        it "Checks the arguments of a function call" $ do
            let ast = scan_and_parse "int a = 5; int tiny(int a, int b) { tiny(a, 5); }"
            checkSemantics ast `shouldBe` Right ()
            let ast = scan_and_parse "int a = 5; int tiny(int a, int b) { tiny(a, c); }"
            checkSemantics ast `shouldBe` Left (SemanticError {errorType = NotDeclaredError, errorVariable = "c"})
        it "Checks that variables are declared before use in a length expression and that the variable is an array" $ do
            let ast = scan_and_parse "int tiny() { length a; }"
            checkSemantics ast `shouldBe` Left (SemanticError NotDeclaredError "a")
            let ast2 = scan_and_parse "int a; int tiny() { length a; }"
            checkSemantics ast2 `shouldBe` Left (SemanticError NotAnArrayError "a")
            let ast3 = scan_and_parse "int a[5]; int tiny() { length a; }"
            checkSemantics ast3 `shouldBe` Right ()
        it "Checks that only scalar expressions are used in binary and unary operations" $ do
            let ast = scan_and_parse "int a[5]; int tiny() { a + 5; }"
            checkSemantics ast `shouldBe` Left (SemanticError NotAScalarError "Var (Name \"a\")")
            let ast2 = scan_and_parse "int a[5]; int tiny() { a[2] + 5; }"
            checkSemantics ast2 `shouldBe` Right ()
            let ast3 = scan_and_parse "int a = 5; int tiny() { a + 5; }"
            checkSemantics ast3 `shouldBe` Right ()
        it "Checks that name subscriptions are used with an array" $ do
            let ast = scan_and_parse "int a = 5; int tiny() { a[5] + 5; }"
            checkSemantics ast `shouldBe` Left (SemanticError {errorType = NotAnArrayError, errorVariable = "a"})
        it "Checks that names are subscribed with scalar expressions" $ do
            let ast = scan_and_parse "int a[5]; int b[6]; int tiny() { a[b]; }"
            checkSemantics ast `shouldBe` Left (SemanticError {errorType = NotAScalarError, errorVariable = "Var (Name \"b\")"})
            let ast2 = scan_and_parse "int a[5]; int b[6]; int tiny() { a[b[3]]; }"
            checkSemantics ast2 `shouldBe` Right ()

