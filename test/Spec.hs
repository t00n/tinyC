import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate, ErrorCall(..))
import System.IO

import qualified Data.Tree as T
import qualified Data.Map as M

import Scanner
import Parser
import Semantics
import TACGenerator
import MonadNames
import NASMGenerator
import qualified SymbolTable as ST

scan_and_parse = parse . alexScanTokens

scan_parse_check xs = do
    let ast = scan_and_parse xs 
    check <- checkSemantics ast
    if check == Right ast then return ast
    else error $ show check

scan_to_tac s = do
    ast <- scan_parse_check s
    return $ generateTAC ast

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
        testTokens "trivial.c" [TokenWrapper INT (0,1,1),TokenWrapper (NAME "a") (4,1,5),TokenWrapper SEMICOLON (5,1,6),TokenWrapper CHAR (8,3,1),TokenWrapper (NAME "c") (13,3,6),TokenWrapper SEMICOLON (14,3,7),TokenWrapper INT (17,5,1),TokenWrapper (NAME "a") (21,5,5),TokenWrapper ASSIGN (23,5,7),TokenWrapper (NUMBER 2) (25,5,9),TokenWrapper SEMICOLON (26,5,10),TokenWrapper CHAR (29,7,1),TokenWrapper (NAME "c") (34,7,6),TokenWrapper ASSIGN (36,7,8),TokenWrapper (QCHAR 'b') (38,7,10),TokenWrapper SEMICOLON (41,7,13),TokenWrapper INT (70,11,1),TokenWrapper (NAME "as") (74,11,5),TokenWrapper LBRACK (76,11,7),TokenWrapper (NUMBER 10) (77,11,8),TokenWrapper RBRACK (79,11,10),TokenWrapper SEMICOLON (80,11,11),TokenWrapper IF (83,13,1),TokenWrapper LPAR (86,13,4),TokenWrapper (NAME "a") (87,13,5),TokenWrapper LESS (89,13,7),TokenWrapper (NUMBER 5) (91,13,9),TokenWrapper RPAR (92,13,10),TokenWrapper LBRACE (94,13,12),TokenWrapper RBRACE (97,15,1),TokenWrapper IF (100,17,1),TokenWrapper LPAR (103,17,4),TokenWrapper (NAME "a") (104,17,5),TokenWrapper GREATER (106,17,7),TokenWrapper (NUMBER 5) (108,17,9),TokenWrapper RPAR (109,17,10),TokenWrapper LBRACE (111,17,12),TokenWrapper RBRACE (114,19,1),TokenWrapper ELSE (116,20,1),TokenWrapper IF (121,20,6),TokenWrapper LPAR (124,20,9),TokenWrapper (NAME "a") (125,20,10),TokenWrapper EQUAL (127,20,12),TokenWrapper (NUMBER 5) (130,20,15),TokenWrapper RPAR (131,20,16),TokenWrapper LBRACE (133,20,18),TokenWrapper RBRACE (136,22,1),TokenWrapper ELSE (138,23,1),TokenWrapper IF (143,23,6),TokenWrapper LPAR (146,23,9),TokenWrapper (NAME "a") (147,23,10),TokenWrapper NEQUAL (149,23,12),TokenWrapper (NUMBER 5) (152,23,15),TokenWrapper RPAR (153,23,16),TokenWrapper LBRACE (155,23,18),TokenWrapper RBRACE (158,25,1),TokenWrapper ELSE (160,26,1),TokenWrapper LBRACE (165,26,6),TokenWrapper RBRACE (168,28,1),TokenWrapper (NAME "a") (170,29,1),TokenWrapper ASSIGN (172,29,3),TokenWrapper (NUMBER 5) (174,29,5),TokenWrapper TIMES (175,29,6),TokenWrapper (NUMBER 4) (176,29,7),TokenWrapper SEMICOLON (177,29,8),TokenWrapper (NAME "a") (179,30,1),TokenWrapper ASSIGN (181,30,3),TokenWrapper (NUMBER 20) (183,30,5),TokenWrapper DIVIDE (186,30,8),TokenWrapper (NUMBER 2) (187,30,9),TokenWrapper SEMICOLON (188,30,10),TokenWrapper (NAME "a") (190,31,1),TokenWrapper ASSIGN (192,31,3),TokenWrapper (NUMBER 5) (194,31,5),TokenWrapper PLUS (195,31,6),TokenWrapper (NUMBER 6) (197,31,8),TokenWrapper SEMICOLON (198,31,9),TokenWrapper (NAME "a") (200,32,1),TokenWrapper ASSIGN (202,32,3),TokenWrapper (NUMBER 8) (204,32,5),TokenWrapper MINUS (206,32,7),TokenWrapper (NUMBER 5) (208,32,9),TokenWrapper SEMICOLON (209,32,10),TokenWrapper (NAME "a") (212,34,1),TokenWrapper ASSIGN (214,34,3),TokenWrapper NOT (216,34,5),TokenWrapper (NUMBER 1) (217,34,6),TokenWrapper SEMICOLON (218,34,7),TokenWrapper (NAME "a") (220,35,1),TokenWrapper ASSIGN (222,35,3),TokenWrapper MINUS (224,35,5),TokenWrapper (NUMBER 5) (225,35,6),TokenWrapper SEMICOLON (226,35,7),TokenWrapper (NAME "as") (229,37,1),TokenWrapper LBRACK (231,37,3),TokenWrapper (NUMBER 5) (232,37,4),TokenWrapper RBRACK (233,37,5),TokenWrapper ASSIGN (235,37,7),TokenWrapper (NUMBER 5) (237,37,9),TokenWrapper SEMICOLON (238,37,10),TokenWrapper WRITE (241,39,1),TokenWrapper LPAR (246,39,6),TokenWrapper (QSTRING "lololol") (247,39,7),TokenWrapper RPAR (256,39,16),TokenWrapper SEMICOLON (257,39,17),TokenWrapper READ (260,41,1),TokenWrapper LPAR (264,41,5),TokenWrapper RPAR (265,41,6),TokenWrapper SEMICOLON (266,41,7),TokenWrapper LENGTH (269,43,1),TokenWrapper LPAR (275,43,7),TokenWrapper (NAME "as") (276,43,8),TokenWrapper RPAR (278,43,10),TokenWrapper SEMICOLON (279,43,11),TokenWrapper INT (282,45,1),TokenWrapper (NAME "i") (286,45,5),TokenWrapper ASSIGN (288,45,7),TokenWrapper (NUMBER 0) (290,45,9),TokenWrapper SEMICOLON (291,45,10),TokenWrapper WHILE (294,47,1),TokenWrapper LPAR (300,47,7),TokenWrapper (NAME "i") (301,47,8),TokenWrapper LESS (303,47,10),TokenWrapper (NUMBER 10) (305,47,12),TokenWrapper RPAR (307,47,14),TokenWrapper LBRACE (309,47,16),TokenWrapper WRITE (315,48,5),TokenWrapper LPAR (320,48,10),TokenWrapper (NAME "i") (321,48,11),TokenWrapper RPAR (322,48,12),TokenWrapper SEMICOLON (323,48,13),TokenWrapper RBRACE (325,49,1),TokenWrapper INT (328,51,1),TokenWrapper (NAME "caca") (332,51,5),TokenWrapper LPAR (336,51,9),TokenWrapper RPAR (337,51,10),TokenWrapper LBRACE (339,51,12),TokenWrapper RETURN (345,52,5),TokenWrapper (NUMBER 42) (352,52,12),TokenWrapper SEMICOLON (354,52,14),TokenWrapper RBRACE (356,53,1),TokenWrapper CHAR (359,55,1),TokenWrapper (NAME "lol") (364,55,6),TokenWrapper LPAR (367,55,9),TokenWrapper INT (368,55,10),TokenWrapper (NAME "n") (372,55,14),TokenWrapper RPAR (373,55,15),TokenWrapper LBRACE (375,55,17),TokenWrapper RETURN (381,56,5),TokenWrapper (NAME "n") (388,56,12),TokenWrapper SEMICOLON (389,56,13),TokenWrapper RBRACE (391,57,1),TokenWrapper INT (394,59,1),TokenWrapper (NAME "lol2") (398,59,5),TokenWrapper LPAR (402,59,9),TokenWrapper INT (403,59,10),TokenWrapper (NAME "a") (407,59,14),TokenWrapper COMMA (408,59,15),TokenWrapper INT (410,59,17),TokenWrapper (NAME "b") (414,59,21),TokenWrapper COMMA (415,59,22),TokenWrapper CHAR (417,59,24),TokenWrapper (NAME "c") (422,59,29),TokenWrapper RPAR (423,59,30),TokenWrapper LBRACE (425,59,32),TokenWrapper RETURN (431,60,5),TokenWrapper (QSTRING "haha") (438,60,12),TokenWrapper SEMICOLON (444,60,18),TokenWrapper RBRACE (446,61,1)]
        testTokens "fibonacci.c" [TokenWrapper INT (1,2,1),TokenWrapper (NAME "fibonacci") (5,2,5),TokenWrapper LPAR (14,2,14),TokenWrapper INT (15,2,15),TokenWrapper (NAME "n") (19,2,19),TokenWrapper RPAR (20,2,20),TokenWrapper LBRACE (22,2,22),TokenWrapper IF (28,3,5),TokenWrapper LPAR (31,3,8),TokenWrapper (NAME "n") (32,3,9),TokenWrapper LESS (34,3,11),TokenWrapper (NUMBER 0) (36,3,13),TokenWrapper RPAR (37,3,14),TokenWrapper LBRACE (39,3,16),TokenWrapper RETURN (49,4,9),TokenWrapper MINUS (56,4,16),TokenWrapper (NUMBER 1) (57,4,17),TokenWrapper SEMICOLON (58,4,18),TokenWrapper RBRACE (64,5,5),TokenWrapper ELSE (70,6,5),TokenWrapper IF (75,6,10),TokenWrapper LPAR (78,6,13),TokenWrapper (NAME "n") (79,6,14),TokenWrapper LESS (81,6,16),TokenWrapper (NUMBER 1) (83,6,18),TokenWrapper RPAR (84,6,19),TokenWrapper LBRACE (86,6,21),TokenWrapper RETURN (96,7,9),TokenWrapper (NUMBER 0) (103,7,16),TokenWrapper SEMICOLON (104,7,17),TokenWrapper RBRACE (110,8,5),TokenWrapper ELSE (116,9,5),TokenWrapper IF (121,9,10),TokenWrapper LPAR (124,9,13),TokenWrapper (NAME "n") (125,9,14),TokenWrapper LESS (127,9,16),TokenWrapper (NUMBER 2) (129,9,18),TokenWrapper RPAR (130,9,19),TokenWrapper LBRACE (132,9,21),TokenWrapper RETURN (142,10,9),TokenWrapper (NUMBER 1) (149,10,16),TokenWrapper SEMICOLON (150,10,17),TokenWrapper RBRACE (156,11,5),TokenWrapper ELSE (162,12,5),TokenWrapper LBRACE (167,12,10),TokenWrapper RETURN (177,13,9),TokenWrapper (NAME "fibonacci") (184,13,16),TokenWrapper LPAR (193,13,25),TokenWrapper (NAME "n") (194,13,26),TokenWrapper MINUS (195,13,27),TokenWrapper (NUMBER 1) (196,13,28),TokenWrapper RPAR (197,13,29),TokenWrapper PLUS (199,13,31),TokenWrapper (NAME "fibonacci") (201,13,33),TokenWrapper LPAR (210,13,42),TokenWrapper (NAME "n") (211,13,43),TokenWrapper MINUS (212,13,44),TokenWrapper (NUMBER 2) (213,13,45),TokenWrapper RPAR (214,13,46),TokenWrapper SEMICOLON (215,13,47),TokenWrapper RBRACE (221,14,5),TokenWrapper RBRACE (223,15,1),TokenWrapper INT (226,17,1),TokenWrapper (NAME "tiny") (230,17,5),TokenWrapper LPAR (234,17,9),TokenWrapper RPAR (235,17,10),TokenWrapper LBRACE (237,17,12),TokenWrapper INT (243,18,5),TokenWrapper (NAME "i") (247,18,9),TokenWrapper ASSIGN (249,18,11),TokenWrapper (NUMBER 0) (251,18,13),TokenWrapper SEMICOLON (252,18,14),TokenWrapper WHILE (258,19,5),TokenWrapper LPAR (264,19,11),TokenWrapper (NAME "i") (265,19,12),TokenWrapper LESS (267,19,14),TokenWrapper (NUMBER 10) (269,19,16),TokenWrapper RPAR (271,19,18),TokenWrapper LBRACE (273,19,20),TokenWrapper WRITE (283,20,9),TokenWrapper (NAME "fibonacci") (289,20,15),TokenWrapper LPAR (298,20,24),TokenWrapper (NAME "i") (299,20,25),TokenWrapper RPAR (300,20,26),TokenWrapper SEMICOLON (301,20,27),TokenWrapper (NAME "i") (311,21,9),TokenWrapper ASSIGN (313,21,11),TokenWrapper (NAME "i") (315,21,13),TokenWrapper PLUS (317,21,15),TokenWrapper (NUMBER 1) (319,21,17),TokenWrapper SEMICOLON (320,21,18),TokenWrapper RBRACE (326,22,5),TokenWrapper RBRACE (328,23,1)]
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
        it "Throws an parse error" $ do
            let ast = scan_and_parse "int tiny() { read x }"
            evaluate ast `shouldThrow` errorCall "Parse error at line 1 column 21 : \"}\""
    describe "Symbol table construction" $ do
        it "Constructs a one-level symbol table" $ do
            let ast = scan_and_parse "char a; int b = 5; int c[5]; char f() {} int g(int a) {}"
            ST.constructST ast `shouldBe` Right (T.Node (M.fromList [("a",ST.VarInfo CharType ST.Scalar 1),("b",ST.VarInfo IntType ST.Scalar 1),("c",ST.VarInfo IntType ST.Array 5),("f",ST.FuncInfo CharType []),("g",ST.FuncInfo IntType [ST.VarInfo IntType ST.Scalar 1])]) [])
    describe "Semantics" $ do
        it "Checks that variables with same name are declared only once on a certain scope level" $ do
            let ast = scan_and_parse "int tiny() { int a; int a; }"
            checkSemantics ast >>= (`shouldBe` Left (SemanticError NameExistsError "a"))
        it "Checks that variables with same name can be declared more than once a different scope level with a warning" $ do
            let ast = scan_and_parse "int a; int tiny() { int a; }"
            checkSemantics ast >>= (`shouldBe` Left (SemanticError NameExistsWarning "a"))
        it "Checks that variables are declared before use in assignment" $ do
            let ast = scan_and_parse "int tiny() { a = 5; }"
            checkSemantics ast >>= (`shouldBe` Left (SemanticError NotDeclaredError "a"))
            let ast = scan_and_parse "int a = 5; int tiny() { a = 3; }"
            checkSemantics ast >>= (`shouldBe` Right ast)
        it "Checks that variables are declared before use in if" $ do
            let ast = scan_and_parse "int tiny() { if (a) a = 5; }"
            checkSemantics ast >>= (`shouldBe` Left (SemanticError NotDeclaredError "a"))
            let ast = scan_and_parse "int a = 5; int tiny() { int c = 3; if (a) a = 3; }"
            checkSemantics ast >>= (`shouldBe` Right ast)
        it "Checks that variables are declared before use in while" $ do
            let ast = scan_and_parse "int tiny() { while (a) a = 5; }"
            checkSemantics ast >>= (`shouldBe` Left (SemanticError NotDeclaredError "a"))
            let ast = scan_and_parse "int a = 5; int tiny() { while (a) a = 5; }"
            checkSemantics ast >>= (`shouldBe` Right ast)
        it "Checks that variables are declared before use in return" $ do
            let ast = scan_and_parse "int tiny() { return a; }"
            checkSemantics ast >>= (`shouldBe` Left (SemanticError NotDeclaredError "a"))
            let ast = scan_and_parse "int a = 5; int tiny() { return a; }"
            checkSemantics ast >>= (`shouldBe` Right ast)
        it "Checks that variables are declared and scalar before use in IO" $ do
            let ast = scan_and_parse "int tiny() { write a; }"
            checkSemantics ast >>= (`shouldBe` Left (SemanticError NotDeclaredError "a"))
            let ast = scan_and_parse "int a = 5; int tiny() { read a; }"
            checkSemantics ast >>= (`shouldBe` Right ast)
            let ast = scan_and_parse "int a = 5; int tiny() { write a; }"
            checkSemantics ast >>= (`shouldBe` Right ast)
            let ast = scan_and_parse "int a[5]; int tiny() { write a; }"
            checkSemantics ast >>= (`shouldBe`  Left (SemanticError {errorType = NotAScalarError, errorVariable = "Var (Name \"a\")"}))
            let ast = scan_and_parse "int a[5]; int tiny() { read a; }"
            checkSemantics ast >>= (`shouldBe`  Left (SemanticError {errorType = NotAScalarError, errorVariable = "a"}))
            let ast = scan_and_parse "int a; int tiny() { read a[5]; }"
            checkSemantics ast >>= (`shouldBe` Left (SemanticError {errorType = NotAnArrayError, errorVariable = "NameSubscription \"a\" (Int 5)"}))
            let ast = scan_and_parse "int a[5]; int tiny() { read a[2]; }"
            checkSemantics ast >>= (`shouldBe` Right ast)
        it "Checks that variables are declared before use in expression" $ do
            let ast = scan_and_parse "int tiny() { a; }"
            checkSemantics ast >>= (`shouldBe` Left (SemanticError NotDeclaredError "a"))
            let ast = scan_and_parse "int a = 5; int tiny() { a; }"
            checkSemantics ast >>= (`shouldBe` Right ast)
        it "Checks that variables are declared before use in binary operations" $ do
            let ast = scan_and_parse "int tiny() { a + 5; }"
            checkSemantics ast >>= (`shouldBe` Left (SemanticError NotDeclaredError "a"))
            let ast = scan_and_parse "int a = 5; int tiny() { a + 5; }"
            checkSemantics ast >>= (`shouldBe` Right ast)
            let ast = scan_and_parse "int tiny() { 5 + a; }"
            checkSemantics ast >>= (`shouldBe` Left (SemanticError NotDeclaredError "a"))
            let ast = scan_and_parse "int a = 5; int tiny() { 5 + a; }"
            checkSemantics ast >>= (`shouldBe` Right ast)
        it "Checks that variables are declared before use in unary operations" $ do
            let ast = scan_and_parse "int tiny() { -a; }"
            checkSemantics ast >>= (`shouldBe` Left (SemanticError NotDeclaredError "a"))
            let ast = scan_and_parse "int a = 5; int tiny() { -a; }"
            checkSemantics ast >>= (`shouldBe` Right ast)
        it "Checks that variables are declared before use in function calls and that the variable is a function" $ do
            let ast = scan_and_parse "int tiny() { a(); }"
            checkSemantics ast >>= (`shouldBe` Left (SemanticError NotDeclaredError "a"))
            let ast = scan_and_parse "int a = 5; int tiny() { a(); }"
            checkSemantics ast >>= (`shouldBe` Left (SemanticError NotAFunctionError "a"))
            let ast = scan_and_parse "int tiny() { tiny(); }"
            checkSemantics ast >>= (`shouldBe` Right ast)
        it "Checks that parameters of a function are declared and variables in the scope" $ do
            let ast = scan_and_parse "int tiny() { int a; a + 5; }"
            checkSemantics ast >>= (`shouldBe` Right ast)
            let ast = scan_and_parse "int tiny() { int a; int a; }"
            checkSemantics ast >>= (`shouldBe` Left (SemanticError {errorType = NameExistsError, errorVariable = "a"}))
        it "Checks the arguments of a function call" $ do
            let ast = scan_and_parse "int a = 5; int f(int a, int b) {} int tiny() { f(a, 5); }"
            checkSemantics ast >>= (`shouldBe` Left (SemanticError {errorType = NameExistsWarning, errorVariable = "a"}))
            let ast = scan_and_parse "int f(int a1, int b) {} int tiny() { int a; f(a, c); }"
            checkSemantics ast >>= (`shouldBe` Left (SemanticError {errorType = NotDeclaredError, errorVariable = "c"}))
            let ast = scan_and_parse "int a[5]; int f(int b) {} int tiny() { f(a); }"
            checkSemantics ast >>= (`shouldBe` Left (SemanticError {errorType = NotAScalarError, errorVariable = "Var (Name \"a\")"}))
            let ast = scan_and_parse "int a; int f(int b[5]) {} int tiny() { f(a); }"
            checkSemantics ast >>= (`shouldBe` Left (SemanticError {errorType = NotAnArrayError, errorVariable = "Var (Name \"a\")"}))
        it "Checks that variables are declared before use in a length expression and that the variable is an array" $ do
            let ast = scan_and_parse "int tiny() { length a; }"
            checkSemantics ast >>= (`shouldBe` Left (SemanticError NotDeclaredError "a"))
            let ast = scan_and_parse "int a; int tiny() { length a; }"
            checkSemantics ast >>= (`shouldBe` Left (SemanticError NotAnArrayError "a"))
            let ast = scan_and_parse "int a[5]; int tiny() { length a; }"
            checkSemantics ast >>= (`shouldBe` Right ast)
        it "Checks that arrays are declared with constant/literals size" $ do
            let ast = scan_and_parse "int a = 5; int b[a]; int tiny() {}"
            checkSemantics ast >>= (`shouldBe` Left (SemanticError {errorType = NotConstantSizeArrayError, errorVariable = "NameSubscription \"b\" (Var (Name \"a\"))"}))
        it "Checks that only scalar expressions are used in binary and unary operations" $ do
            let ast = scan_and_parse "int a[5]; int tiny() { a + 5; }"
            checkSemantics ast >>= (`shouldBe` Left (SemanticError NotAScalarError "Var (Name \"a\")"))
            let ast = scan_and_parse "int a[5]; int tiny() { a[2] + 5; }"
            checkSemantics ast >>= (`shouldBe` Right ast)
            let ast = scan_and_parse "int a = 5; int tiny() { a + 5; }"
            checkSemantics ast >>= (`shouldBe` Right ast)
        it "Checks that name subscriptions are used with an array" $ do
            let ast = scan_and_parse "int a = 5; int tiny() { a[5] + 5; }"
            checkSemantics ast >>= (`shouldBe` Left (SemanticError {errorType = NotAnArrayError, errorVariable = "a"}))
        it "Checks that names are subscribed with scalar expressions" $ do
            let ast = scan_and_parse "int a[5]; int b[6]; int tiny() { a[b]; }"
            checkSemantics ast >>= (`shouldBe` Left (SemanticError {errorType = NotAScalarError, errorVariable = "Var (Name \"b\")"}))
            let ast = scan_and_parse "int a[5]; int b[6]; int tiny() { a[b[3]]; }"
            checkSemantics ast >>= (`shouldBe` Right ast)
        it "Checks that assignments have the same scalarity" $ do
            let ast = scan_and_parse "int a; int b; int tiny() { a = b; }"
            checkSemantics ast >>= (`shouldBe` Right ast)
            let ast = scan_and_parse "int a; int b[5]; int tiny() { a = b; }"
            checkSemantics ast >>= (`shouldBe` Left (SemanticError {errorType = NotSameScalarityError, errorVariable = "Name \"a\" Var (Name \"b\")"}))
            let ast = scan_and_parse "int a[5]; int b; int tiny() { a = b; }"
            checkSemantics ast >>= (`shouldBe` Left (SemanticError {errorType = NotSameScalarityError, errorVariable = "Name \"a\" Var (Name \"b\")"}))
            let ast = scan_and_parse "int a; int b[5]; int tiny() { a = b[2]; }"
            checkSemantics ast >>= (`shouldBe` Right ast)
            let ast = scan_and_parse "int a; int b[5]; int tiny() { b[2] = a; }"
            checkSemantics ast >>= (`shouldBe` Right ast)
            let ast = scan_and_parse "int a[2]; int b[5]; int tiny() { a = b; }"
            checkSemantics ast >>= (`shouldBe` Right ast)
        it "Checks that return statement have scalar expression" $ do
            let ast = scan_and_parse "int tiny() { return 4; }"
            checkSemantics ast >>= (`shouldBe` Right ast)
            let ast = scan_and_parse "int b[5]; int tiny() { return b; }"
            checkSemantics ast >>= (`shouldBe` Left (SemanticError {errorType = NotAScalarError, errorVariable = "Var (Name \"b\")"}))
        it "Checks that if and while statements have scalar expressions" $ do
            let ast = scan_and_parse "int tiny() { while (5) {} if (5) {} }"
            checkSemantics ast >>= (`shouldBe` Right ast)
            let ast = scan_and_parse "int a[5]; int tiny() { while (a) {} }"
            checkSemantics ast >>= (`shouldBe` Left (SemanticError {errorType = NotAScalarError, errorVariable = "Var (Name \"a\")"}))
            let ast = scan_and_parse "int a[5]; int tiny() { if (a) {} }"
            checkSemantics ast >>= (`shouldBe` Left (SemanticError {errorType = NotAScalarError, errorVariable = "Var (Name \"a\")"}))
            let ast = scan_and_parse "int a[5]; int tiny() { if (a) {} else if (a) {} }"
            checkSemantics ast >>= (`shouldBe` Left (SemanticError {errorType = NotAScalarError, errorVariable = "Var (Name \"a\")"}))
        it "Checks that one and only one entry point exists" $ do
            let ast = scan_and_parse "int a;"
            checkSemantics ast >>= (`shouldBe` Left (SemanticError {errorType = NoTinyFunctionError, errorVariable = ""}))
            let ast = scan_and_parse "int tiny() {} int tiny() {}"
            checkSemantics ast >>= (`shouldBe`  Left (SemanticError {errorType = SeveralTinyFunctionError, errorVariable = ""}))
    describe "The generation of three-address-code" $ do
        it "Generates a few declarations" $ do
            ast <- scan_parse_check "int a; int b; int tiny() {}"
            generateTAC ast `shouldBe` [TACDeclaration (TACVar "a"),TACDeclaration (TACVar "b"),TACFunction "tiny" [],TACReturn Nothing]
        it "Generates declarations with complex binary expressions" $ do
            ast <- scan_parse_check "int a = 5; int b = (a+5)/(a-2); int tiny() {}"
            generateTAC ast `shouldBe` [TACDeclarationValue (TACVar "a") (TACInt 5),TACBinary "t1" (TACVar "a") TACPlus (TACInt 5),TACBinary "t2" (TACVar "a") TACMinus (TACInt 2),TACBinary "t3" (TACVar "t1") TACDivide (TACVar "t2"),TACDeclarationValue (TACVar "b") (TACVar "t3"),TACFunction "tiny" [],TACReturn Nothing]
        it "Generates function declarations" $ do
            ast <- scan_parse_check "int tiny() {}"
            generateTAC ast `shouldBe` [TACFunction "tiny" [],TACReturn Nothing]
            ast <- scan_parse_check "int f(int a, int b) {int c = 4;} int tiny() { }"
            generateTAC ast `shouldBe` [TACFunction "f" ["a","b"],TACDeclarationValue (TACVar "c") (TACInt 4),TACReturn Nothing,TACFunction "tiny" [],TACReturn Nothing]
        it "Generates declarations with complex unary expressions" $ do
            ast <- scan_parse_check "int a = 5; int b = -(a - 5); int tiny() {}"
            generateTAC ast `shouldBe` [TACDeclarationValue (TACVar "a") (TACInt 5),TACBinary "t1" (TACVar "a") TACMinus (TACInt 5),TACUnary "t2" TACNeg (TACVar "t1"),TACDeclarationValue (TACVar "b") (TACVar "t2"),TACFunction "tiny" [],TACReturn Nothing]
        it "Generates function calls" $ do
            ast <- scan_parse_check "int f(int a, int b) { int c = 1; f(c, 2); } int tiny() {}"
            generateTAC ast `shouldBe` [TACFunction "f" ["a","b"],TACDeclarationValue (TACVar "c") (TACInt 1),TACCall "f" [TACVar "c",TACInt 2,TACVar "t1"],TACReturn Nothing,TACFunction "tiny" [],TACReturn Nothing]
            ast <- scan_parse_check "int f(int a, int b) { int c = f(2 + 3, 1); } int tiny() {}"
            generateTAC ast `shouldBe`  [TACFunction "f" ["a","b"],TACBinary "t1" (TACInt 2) TACPlus (TACInt 3),TACCall "f" [TACVar "t1",TACInt 1,TACVar "t2"],TACDeclarationValue (TACVar "c") (TACVar "t2"),TACReturn Nothing,TACFunction "tiny" [],TACReturn Nothing]
        it "Generates assignments" $ do
            ast <- scan_parse_check "int tiny() { int a; a = (a + 5) * 3; }"
            generateTAC ast `shouldBe` [TACFunction "tiny" [],TACDeclaration (TACVar "a"),TACBinary "t1" (TACVar "a") TACPlus (TACInt 5),TACBinary "t2" (TACVar "t1") TACTimes (TACInt 3),TACCopy "a" (TACVar "t2"),TACReturn Nothing]
            ast <- scan_parse_check "int a[5]; int b = a[2]; int tiny() {}"
            generateTAC ast `shouldBe` [TACDeclaration (TACArray "a" (TACInt 5)),TACArrayAccess "t1" (TACArray "a" (TACInt 2)),TACDeclarationValue (TACVar "b") (TACVar "t1"),TACFunction "tiny" [],TACReturn Nothing]
            ast <- scan_parse_check "int a[5]; int tiny() { a[2] = 5; }"
            generateTAC ast `shouldBe` [TACDeclaration (TACArray "a" (TACInt 5)),TACFunction "tiny" [],TACArrayModif (TACArray "a" (TACInt 2)) (TACInt 5),TACReturn Nothing]
            ast <- scan_parse_check "int a[5]; int b[5]; int tiny() { a[2] = b[3]; }"
            generateTAC ast `shouldBe` [TACDeclaration (TACArray "a" (TACInt 5)),TACDeclaration (TACArray "b" (TACInt 5)),TACFunction "tiny" [],TACArrayAccess "t1" (TACArray "b" (TACInt 3)),TACArrayModif (TACArray "a" (TACInt 2)) (TACVar "t1"),TACReturn Nothing]
        it "Generates if" $ do
            ast <- scan_parse_check "int tiny() { if (1 > 2) { int a = 5; a = 3; } }"
            generateTAC ast `shouldBe` [TACFunction "tiny" [],TACIf (TACExpr (TACInt 1) TACGreater (TACInt 2)) "l1",TACGoto "l2",TACLabel "l1",TACDeclarationValue (TACVar "a") (TACInt 5),TACCopy "a" (TACInt 3),TACLabel "l2",TACReturn Nothing]
        it "Generates if else" $ do
            ast <- scan_parse_check "int tiny() { if (1 > 2) { int a = 5; } else { int b = 5; } }"
            generateTAC ast `shouldBe` [TACFunction "tiny" [],TACIf (TACExpr (TACInt 1) TACGreater (TACInt 2)) "l1",TACGoto "l2",TACLabel "l1",TACDeclarationValue (TACVar "a") (TACInt 5),TACGoto "l3",TACLabel "l2",TACDeclarationValue (TACVar "b") (TACInt 5),TACLabel "l3",TACReturn Nothing]
        it "Generates several if else" $ do
            ast <- scan_parse_check "int tiny() { if (1 > 2) { int a = 1; } else if (2 > 3) { int b = 2; } else { int c = 3; } }"
            generateTAC ast `shouldBe` [TACFunction "tiny" [],TACIf (TACExpr (TACInt 1) TACGreater (TACInt 2)) "l1",TACGoto "l2",TACLabel "l1",TACDeclarationValue (TACVar "a") (TACInt 1),TACGoto "l3",TACLabel "l2",TACIf (TACExpr (TACInt 2) TACGreater (TACInt 3)) "l4",TACGoto "l5",TACLabel "l4",TACDeclarationValue (TACVar "b") (TACInt 2),TACGoto "l6",TACLabel "l5",TACDeclarationValue (TACVar "c") (TACInt 3),TACLabel "l6",TACLabel "l3",TACReturn Nothing]
        it "Generates a while" $ do
            ast <- scan_parse_check "int tiny() { int a = 2; while ( a > 1) { a = a - 1; } }"
            generateTAC ast `shouldBe` [TACFunction "tiny" [],TACDeclarationValue (TACVar "a") (TACInt 2),TACLabel "l1",TACIf (TACExpr (TACVar "a") TACGreater (TACInt 1)) "l3",TACGoto "l2",TACLabel "l3",TACBinary "t1" (TACVar "a") TACMinus (TACInt 1),TACCopy "a" (TACVar "t1"),TACGoto "l1",TACLabel "l2",TACReturn Nothing]
        it "Generates a return" $ do
            ast <- scan_parse_check "int tiny() { int a = 2; return a; }"
            generateTAC ast `shouldBe` [TACFunction "tiny" [],TACDeclarationValue (TACVar "a") (TACInt 2),TACReturn (Just $ TACVar "a")]
            ast <- scan_parse_check "int tiny() { return 3 + 4 / 5; }"
            generateTAC ast `shouldBe` [TACFunction "tiny" [],TACBinary "t1" (TACInt 4) TACDivide (TACInt 5),TACBinary "t2" (TACInt 3) TACPlus (TACVar "t1"),TACReturn (Just $ TACVar "t2")]
        it "Generates a write" $ do
            ast <- scan_parse_check "int tiny() { int a = 2; write a; }"
            generateTAC ast `shouldBe` [TACFunction "tiny" [],TACDeclarationValue (TACVar "a") (TACInt 2),TACWrite (TACVar "a"),TACReturn Nothing]
            ast <- scan_parse_check "int tiny() { int a[5]; write a[2]; }"
            generateTAC ast `shouldBe` [TACFunction "tiny" [],TACDeclaration (TACArray "a" (TACInt 5)),TACArrayAccess "t1" (TACArray "a" (TACInt 2)),TACWrite (TACVar "t1"),TACReturn Nothing]
        it "Generates reads" $ do
            ast <- scan_parse_check "int tiny() { int a; read a; }"
            generateTAC ast `shouldBe` [TACFunction "tiny" [],TACDeclaration (TACVar "a"),TACRead (TACVar "a"),TACReturn Nothing]
            ast <- scan_parse_check "int tiny() { int a[5]; read a[2]; }"
            generateTAC ast `shouldBe` [TACFunction "tiny" [],TACDeclaration (TACArray "a" (TACInt 5)),TACRead (TACArray "a" (TACInt 2)),TACReturn Nothing]
    describe "Do the name generator works ????" $ do
        it "Tests everything" $ do
            evalNames (do { s1 <- popVariable; s2 <- nextVariable; l1 <- nextLabel; return [s1, s2, l1] }) ["t" ++ show i | i <- [1..]] ["l" ++ show i | i <- [1..]] `shouldBe` ["t1", "t2", "l1"]
    describe "Generate NASM code" $ do
        it "Generates data" $ do
            tac <- scan_to_tac "int a; int b = 2; char c = 'a'; int tiny() {}"
            generateNASM tac `shouldBe` ([NASMData "a" DWORDADDRESS [0],NASMData "b" DWORDADDRESS [2],NASMData "c" BYTEADDRESS [97]],[])
