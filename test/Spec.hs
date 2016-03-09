import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate, ErrorCall)
import System.IO

import Scanner
import Parser

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
        testTokens "trivial.c" [INT,NAME "a",SEMICOLON,CHAR,NAME "c",SEMICOLON,INT,NAME "a",ASSIGN,NUMBER 2,SEMICOLON,CHAR,NAME "c",ASSIGN,QCHAR 'b',SEMICOLON,INT,NAME "as",LBRACK,NUMBER 10,RBRACK,SEMICOLON,IF,LPAR,NAME "a",LESS,NUMBER 5,RPAR,LBRACE,RBRACE,IF,LPAR,NAME "a",GREATER,NUMBER 5,RPAR,LBRACE,RBRACE,ELSE,IF,LPAR,NAME "a",EQUAL,NUMBER 5,RPAR,LBRACE,RBRACE,ELSE,IF,LPAR,NAME "a",NEQUAL,NUMBER 5,RPAR,LBRACE,RBRACE,ELSE,LBRACE,RBRACE,NAME "a",ASSIGN,NUMBER 5,TIMES,NUMBER 4,SEMICOLON,NAME "a",ASSIGN,NUMBER 20,DIVIDE,NUMBER 2,SEMICOLON,NAME "a",ASSIGN,NUMBER 5,PLUS,NUMBER 6,SEMICOLON,NAME "a",ASSIGN,NUMBER 8,MINUS,NUMBER 5,SEMICOLON,NAME "a",ASSIGN,NOT,NUMBER 1,SEMICOLON,NAME "a",ASSIGN,MINUS,NUMBER 5,SEMICOLON,NAME "as",LBRACK,NUMBER 5,RBRACK,ASSIGN,NUMBER 5,SEMICOLON,WRITE,LPAR,QString "lololol",RPAR,SEMICOLON,READ,LPAR,RPAR,SEMICOLON,LENGTH,LPAR,NAME "as",RPAR,SEMICOLON,INT,NAME "i",ASSIGN,NUMBER 0,SEMICOLON,WHILE,LPAR,NAME "i",LESS,NUMBER 10,RPAR,LBRACE,WRITE,LPAR,NAME "i",RPAR,SEMICOLON,RBRACE,INT,NAME "caca",LPAR,RPAR,LBRACE,RETURN,NUMBER 42,SEMICOLON,RBRACE,CHAR,NAME "lol",LPAR,INT,NAME "n",RPAR,LBRACE,RETURN,NAME "n",SEMICOLON,RBRACE,INT,NAME "lol2",LPAR,INT,NAME "a",COMMA,INT,NAME "b",COMMA,CHAR,NAME "c",RPAR,LBRACE,RETURN,QString "haha",SEMICOLON,RBRACE]
        testTokens "fibonacci.c" [INT,NAME "fibonacci",LPAR,INT,NAME "n",RPAR,LBRACE,IF,LPAR,NAME "n",LESS,NUMBER 0,RPAR,LBRACE,RETURN,MINUS,NUMBER 1,SEMICOLON,RBRACE,ELSE,IF,LPAR,NAME "n",LESS,NUMBER 1,RPAR,LBRACE,RETURN,NUMBER 0,SEMICOLON,RBRACE,ELSE,IF,LPAR,NAME "n",LESS,NUMBER 2,RPAR,LBRACE,RETURN,NUMBER 1,SEMICOLON,RBRACE,ELSE,LBRACE,RETURN,NAME "fibonacci",LPAR,NAME "n",MINUS,NUMBER 1,RPAR,PLUS,NAME "fibonacci",LPAR,NAME "n",MINUS,NUMBER 2,RPAR,SEMICOLON,RBRACE,RBRACE,INT,NAME "main",LPAR,INT,NAME "argc",COMMA,CHAR,TIMES,TIMES,NAME "argv",RPAR,LBRACE,INT,NAME "i",ASSIGN,NUMBER 0,SEMICOLON,WHILE,LPAR,NAME "i",LESS,NUMBER 10,RPAR,LBRACE,WRITE,LPAR,NAME "fibonacci",LPAR,NAME "i",RPAR,RPAR,SEMICOLON,NAME "i",ASSIGN,NAME "i",PLUS,NUMBER 1,SEMICOLON,RBRACE,RBRACE]
        testTokensThrow "wrong.c" anyErrorCall
    describe "Parser" $ do
        it "Parses int declarations" $ do
            let ast = scan_and_parse "int a = 2; int b;"
            ast `shouldBe` [VarDeclaration IntType "a" (Just $ Int 2), VarDeclaration IntType "b" Nothing]
        it "Parses several declarations with too much semicolons" $ do
            let ast = scan_and_parse "int a = 2;;; int b = 3;; int c = 4;"
            ast `shouldBe` [VarDeclaration IntType "a" (Just $ Int 2), VarDeclaration IntType "b" (Just $ Int 3), VarDeclaration IntType "c" (Just $ Int 4)]
        it "Parses declarations without semicolons and throws an exception" $ do
            let ast = scan_and_parse "int a = 2 int b = 3;"
            evaluate ast `shouldThrow` anyErrorCall
        it "Parses integer expressions" $ do
            let ast = scan_and_parse "int a = 2 + 3; int b = 2 + 3 * 4; int c = 2 / 4 + 3 * 2; int d = 8/2 + 1;"
            ast `shouldBe` [VarDeclaration IntType "a" (Just $ BinOp (Int 2) Plus (Int 3)), VarDeclaration IntType "b" (Just $ BinOp (Int 2) Plus $ BinOp (Int 3) Times (Int 4)), VarDeclaration IntType "c" (Just $ BinOp (BinOp (Int 2) Divide (Int 4)) Plus (BinOp (Int 3) Times (Int 2))), VarDeclaration IntType "d" (Just $ BinOp (BinOp (Int 8) Divide (Int 2)) Plus (Int 1))]
        it "Parses unary operators" $ do
            let ast = scan_and_parse "int a = -5; int b = -7 + -5; int c = !5;"
            ast `shouldBe` [VarDeclaration IntType "a" (Just $ UnOp Neg (Int 5)), VarDeclaration IntType "b" (Just $ BinOp (UnOp Neg (Int 7)) Plus (UnOp Neg (Int 5))), VarDeclaration IntType "c" (Just $ UnOp Not (Int 5))]
        it "Parses declaration of int with char expressions" $ do
            let ast = scan_and_parse "int a = 'c'; char c = 5;"
            ast `shouldBe` [VarDeclaration IntType "a" (Just $ Char 'c'), VarDeclaration CharType "c" (Just $ Int 5)]
        it "Parses integer and char expressions" $ do
            let ast = scan_and_parse "int c = 'c' + 'b'; int a = 2 + 'c'; char c = 'c' + 2; char c = 2 + 3;"
            ast `shouldBe` [VarDeclaration IntType "c" (Just $ BinOp (Char 'c') Plus (Char 'b')), VarDeclaration IntType "a" (Just $ BinOp (Int 2) Plus (Char 'c')), VarDeclaration CharType "c" (Just $ BinOp (Char 'c') Plus (Int 2)), VarDeclaration CharType "c" (Just $ BinOp (Int 2) Plus (Int 3))]
        it "Parses function declarations with no args and empty block" $ do
            let ast = scan_and_parse "int tiny() { }"
            ast `shouldBe` [FuncDeclaration IntType "tiny" [] (Block [] [])]
        it "Parses function declaration with one arg and empty block" $ do
            let ast = scan_and_parse "int tiny(int a) { }"
            ast `shouldBe` [FuncDeclaration IntType "tiny" [Parameter IntType "a"] (Block [] [])]
        it "Parses function declaration with two args and empty block" $ do
            let ast = scan_and_parse "int tiny(int a, char b) { }"
            ast `shouldBe` [FuncDeclaration IntType "tiny" [Parameter IntType "a", Parameter CharType "b"] (Block [] [])]
        it "Parses function declaration with no args and block with too much semicolons" $ do
            let ast = scan_and_parse "int tiny() { ;;; }"
            ast `shouldBe` [FuncDeclaration IntType "tiny" [] (Block [] [])]
        it "Parses function declarations with declarations" $ do
            let ast = scan_and_parse "int tiny(int a) { int b = 2; }"
            ast `shouldBe` [FuncDeclaration IntType "tiny" [Parameter IntType "a"] (Block [VarDeclaration IntType "b" (Just $ Int 2)] [])]
        it "Parses function declaration with declarations and too much semicolons" $ do
            let ast = scan_and_parse "int tiny() { ;; int b = 2;; int c = 3;;;}"
            ast `shouldBe` [FuncDeclaration IntType "tiny" [] (Block [VarDeclaration IntType "b" (Just $ Int 2), VarDeclaration IntType "c" (Just $ Int 3)] [])]
        it "Parses function declaration with one statement" $ do
            let ast = scan_and_parse "int tiny() { b = 3; }"
            ast `shouldBe` [FuncDeclaration IntType "tiny" [] (Block [] [Assignment "b" (Int 3)])]
        it "Parses function declaration with several statements" $ do
            let ast = scan_and_parse "int tiny() { b = 3; c = 4; }"
            ast `shouldBe` [FuncDeclaration IntType "tiny" [] (Block [] [Assignment "b" (Int 3), Assignment "c" (Int 4)])]
        it "Parses function declaration with several statements and too much semicolons" $ do
            let ast = scan_and_parse "int tiny() { ;;;b = 3;; c = 4;; }"
            ast `shouldBe` [FuncDeclaration IntType "tiny" [] (Block [] [Assignment "b" (Int 3), Assignment "c" (Int 4)])]
        it "Parses function declaration with several declarations and statements" $ do
            let ast = scan_and_parse "int tiny() { int a = 4; int b; b = 3; c = 4; }"
            ast `shouldBe` [FuncDeclaration IntType "tiny" [] (Block [VarDeclaration IntType "a" (Just $ Int 4), VarDeclaration IntType "b" Nothing] [Assignment "b" (Int 3), Assignment "c" (Int 4)])]
        it "Parses function declaration with statements before declarations and throws an exception" $ do
            let ast = scan_and_parse "int tiny() { a = 4; int b; b = 3; c = 4; }"
            evaluate ast `shouldThrow` anyErrorCall
        it "Parses an if with one instruction " $ do
            let ast = scan_and_parse "int tiny() { if (a == 5) a = 3; }"
            ast `shouldBe` [FuncDeclaration IntType "tiny" [] (Block [] [If (BinOp (Var "a") Equal (Int 5)) (Assignment "a" (Int 3))])]
        it "Parses an if block" $ do
            let ast = scan_and_parse "int tiny() { if (a == 5) { a = 3; b = 'c';} }"
            ast `shouldBe` [FuncDeclaration IntType "tiny" [] (Block [] [If (BinOp (Var "a") Equal (Int 5)) (Block [] [Assignment "a" (Int 3), Assignment "b" (Char 'c')])])]
        it "Parses a while with one instruction" $ do
            let ast = scan_and_parse "int tiny() { while (a == 5) a = 3; b = 2; }"
            ast `shouldBe` [FuncDeclaration IntType "tiny" [] (Block [] [While (BinOp (Var "a") Equal (Int 5)) (Assignment "a" (Int 3)), Assignment "b" (Int 2)])]
        it "Parses a while block" $ do
            let ast = scan_and_parse "int tiny() { while (a == 5) { int b = 'c'; a = 4; } }"
            ast `shouldBe` [FuncDeclaration IntType "tiny" [] (Block [] [While (BinOp (Var "a") Equal (Int 5)) (Block [VarDeclaration IntType "b" (Just $ Char 'c')] [Assignment "a" (Int 4)])])]


        --it "Parses assignments of int and chars" $ do
        --    let ast = scan_and_parse "a = 5; b = 'c';"
        --    ast `shouldBe`[Assignment "a" (Int 5), Assignment "b" (Char 'c')]
        --it "Uses variables in declarations and assignments" $ do
        --    let ast = scan_and_parse "int a = b; char c = a + 5; a = 'c' + c; b = a + b;"
        --    ast `shouldBe` [VarDeclaration IntType "a" (Just $ Var "b"), VarDeclaration CharType "c" (Just $ BinOp (Var "a") Plus (Int 5)), Assignment "a" (BinOp (Char 'c') Plus (Var "c")), Assignment "b" (BinOp (Var "a") Plus (Var "b"))]

