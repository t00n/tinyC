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
            let s = "int a = 2; int b;"
            let tokens = alexScanTokens s
            let ast = parse tokens
            ast `shouldBe` [Declaration IntType "a" (Just $ Int 2), Declaration IntType "b" Nothing]
        it "Parses several declarations with too much semicolons" $ do
            let s = "int a = 2;;; int b = 3;; int c = 4;"
            let tokens = alexScanTokens s
            let ast = parse tokens
            ast `shouldBe` [Declaration IntType "a" (Just $ Int 2), Declaration IntType "b" (Just $ Int 3), Declaration IntType "c" (Just $ Int 4)]
        it "Parses declarations without semicolons and throws an exception" $ do
            let s = "int a = 2 int b = 3;"
            let tokens = alexScanTokens s
            evaluate (parse tokens) `shouldThrow` anyErrorCall
        it "Parses integer expressions" $ do
            let s = "int a = 2 + 3; int b = 2 + 3 * 4; int c = 2 / 4 + 3 * 2; int d = 8/2 + 1;"
            let tokens = alexScanTokens s
            let ast = parse tokens
            ast `shouldBe` [Declaration IntType "a" (Just $ Operator (Int 2) Plus (Int 3)), Declaration IntType "b" (Just $ Operator (Int 2) Plus $ Operator (Int 3) Times (Int 4)), Declaration IntType "c" (Just $ Operator (Operator (Int 2) Divide (Int 4)) Plus (Operator (Int 3) Times (Int 2))), Declaration IntType "d" (Just $ Operator (Operator (Int 8) Divide (Int 2)) Plus (Int 1))]
        it "Parses declaration of int with char expressions" $ do
            let s = "int a = 'c'; char c = 5;"
            let tokens = alexScanTokens s
            let ast = parse tokens
            ast `shouldBe` [Declaration IntType "a" (Just $ Char 'c'), Declaration CharType "c" (Just $ Int 5)]
        it "Parses integer and char expressions" $ do
            let s = "int c = 'c' + 'b'; int a = 2 + 'c'; char c = 'c' + 2; char c = 2 + 3;"
            let tokens = alexScanTokens s
            let ast = parse tokens
            ast `shouldBe` [Declaration IntType "c" (Just $ Operator (Char 'c') Plus (Char 'b')), Declaration IntType "a" (Just $ Operator (Int 2) Plus (Char 'c')), Declaration CharType "c" (Just $ Operator (Char 'c') Plus (Int 2)), Declaration CharType "c" (Just $ Operator (Int 2) Plus (Int 3))]
        it "Parses assignments of int and chars" $ do
            let s = "a = 5; b = 'c';"
            let tokens = alexScanTokens s
            let ast = parse tokens
            ast `shouldBe`[Assignment "a" (Int 5), Assignment "b" (Char 'c')]
        it "Uses variables in declarations and assignments" $ do
            let ast = (parse . alexScanTokens) "int a = b; char c = a + 5; a = 'c' + c; b = a + b;"
            ast `shouldBe` [Declaration IntType "a" (Just $ Var "b"), Declaration CharType "c" (Just $ Operator (Var "a") Plus (Int 5)), Assignment "a" (Operator (Char 'c') Plus (Var "c")), Assignment "b" (Operator (Var "a") Plus (Var "b"))]
        it "Parses an if with one instruction " $ do
            let ast = scan_and_parse "if (a == 5) a = 3;"
            ast `shouldBe` [If (Operator (Var "a") Equal (Int 5)) (Assignment "a" (Int 3))]
        it "Parses an if block" $ do
            let ast2 = scan_and_parse "if (a == 5) { a = 3; }"
            ast2 `shouldBe` [IfBlock (Operator (Var "a") Equal (Int 5)) [Assignment "a" (Int 3)]]

