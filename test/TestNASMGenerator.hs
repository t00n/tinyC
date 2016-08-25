module TestNASMGenerator (testNASMGenerator) where

import Test.Hspec

import TestParser(scan_and_parse)
import TestTACGenerator (scan_parse_check)

import qualified Data.Map as M

import Semantics
import TACGenerator
import TACAnalysis
import NASMGenerator


testNASMGenerator = 
	describe "Generate NASM code" $ do
        it "Generates data" $ do
            let ast = scan_and_parse "int a; int b = 2; char c = 'a'; int v[5]; int tiny() {}"
            let st = symbolTable ast
            let tac = tacGenerate st ast
            nasmGenerateData tac st `shouldBe` [NASMData "a" DD [0],NASMData "b" DD [2],NASMData "c" DB [97],NASMData "v" DD [0,0,0,0,0]]
            let ast = scan_and_parse "int a; int tiny() { int b; } int c;"
            let st = symbolTable ast
            let tac = tacGenerate st ast
            nasmGenerateData tac st `shouldBe` [NASMData "a" DD [0],NASMData "c" DD [0]]
        it "Generates a simple tiny function" $ do
            let ast = scan_and_parse "int tiny() {}"
            let st = symbolTable ast
            let tac = tacGenerate st ast
            nasmGenerateText tac st `shouldBe` [LABEL "tiny",PUSH1 BP,MOV1 DWORD BP SP,CALL "_exit"]
        it "Generates two simple functions" $ do
            let ast = scan_and_parse "int tiny() {} int f() {}"
            let st = symbolTable ast
            let tac = tacGenerate st ast
            nasmGenerateText tac st `shouldBe` [LABEL "tiny",PUSH1 BP,MOV1 DWORD BP SP,CALL "_exit",LABEL "f",PUSH1 BP,MOV1 DWORD BP SP,PUSH1 B,PUSH1 SI,PUSH1 DI,POP1 DI,POP1 SI,POP1 B,MOV1 DWORD SP BP,POP1 BP,RET]
        it "Generates a few declarations and computations" $ do
            let ast = scan_and_parse "int a; int tiny() { int b = 2; int c = 3; int d = (a+b)/(b-c); return d; }"
            let st = symbolTable ast
            let tac = tacGenerate st ast
            nasmGenerate tac st `shouldBe` NASMProgram [NASMData "a" DD [0]] [LABEL "tiny",PUSH1 BP,MOV1 DWORD BP SP,MOV4 (Register C DWORD) 2,MOV4 (Register D DWORD) 3,MOV2 (Register A DWORD) (AddressLabelOffset "a" 0 1),ADD1 DWORD A C,SUB1 DWORD C D,PUSH1 D,XOR1 DWORD D D,IDIV1 C,POP1 D,CALL "_exit"]
        it "Generates functions with arguments and calling convention" $ do
            let ast = scan_parse_check "int g() {} int tiny() { int a = 2; f(5, a); } int f(int x, int y) { return x + y; }"
            let st = symbolTable ast
            let tac = tacGenerate st ast
            nasmGenerate tac st `shouldBe` NASMProgram [] [LABEL "g",PUSH1 BP,MOV1 DWORD BP SP,PUSH1 B,PUSH1 SI,PUSH1 DI,POP1 DI,POP1 SI,POP1 B,MOV1 DWORD SP BP,POP1 BP,RET,LABEL "tiny",PUSH1 BP,MOV1 DWORD BP SP,MOV4 (Register A DWORD) 2,PUSH1 A,PUSH1 C,PUSH1 D,PUSH1 A,PUSH3 5,CALL "f",ADD4 (Register SP DWORD) 8,POP1 D,POP1 C,POP1 A,CALL "_exit",LABEL "f",PUSH1 BP,MOV1 DWORD BP SP,PUSH1 B,PUSH1 SI,PUSH1 DI,MOV2 (Register A DWORD) (AddressRegisterOffset (Register BP DWORD) 8 1),MOV2 (Register C DWORD) (AddressRegisterOffset (Register BP DWORD) 12 1),ADD1 DWORD A C,POP1 DI,POP1 SI,POP1 B,MOV1 DWORD SP BP,POP1 BP,RET]
        it "Generates code for bigprogram.c" $ do
            code <- readFile "test/fixtures/bigprogram.c"
            let ast = scan_parse_check code
            let st = symbolTable ast
            let tac = tacGenerate st ast
            nasmGenerate tac st `shouldBe` NASMProgram [NASMData "a" DD [5],NASMData "b" DD [2],NASMData "c" DD [4],NASMData "d" DD [3]] [LABEL "tiny",PUSH1 BP,MOV1 DWORD BP SP,SUB4 (Register SP DWORD) 4,MOV2 (Register C DWORD) (AddressLabelOffset "b" 0 1),MOV2 (Register A DWORD) (AddressLabelOffset "a" 0 1),MOV1 DWORD SI A,IMUL4 SI C,MOV2 (Register D DWORD) (AddressLabelOffset "c" 0 1),MOV1 DWORD DI C,SUB1 DWORD DI D,PUSH1 D,XOR1 DWORD D D,PUSH1 A,MOV1 DWORD A SI,IDIV1 DI,MOV1 DWORD SI A,POP1 A,POP1 D,MOV2 (Register B DWORD) (AddressLabelOffset "d" 0 1),MOV1 DWORD DI B,IMUL4 DI A,MOV1 DWORD B D,IMUL4 B C,MOV3 (AddressRegisterOffset (Register BP DWORD) (-4) 1) (Register B DWORD),MOV2 (Register B DWORD) (AddressRegisterOffset (Register BP DWORD) (-4) 1),ADD1 DWORD DI B,PUSH1 D,XOR1 DWORD D D,PUSH1 A,MOV1 DWORD A SI,IDIV1 DI,MOV1 DWORD SI A,POP1 A,POP1 D,MOV1 DWORD A A,IMUL4 A D,MOV2 (Register B DWORD) (AddressLabelOffset "d" 0 1),SUB1 DWORD C B,PUSH1 D,XOR1 DWORD D D,IDIV1 C,POP1 D,ADD1 DWORD A SI,PUSH1 A,PUSH1 C,PUSH1 D,PUSH1 A,CALL "_writeint",ADD4 (Register SP DWORD) 4,POP1 D,POP1 C,POP1 A,CALL "_exit"]
        it "Generates code for fibonacci.c" $ do
            code <- readFile "test/fixtures/fibonacci.c"
            let ast = scan_parse_check code
            let st = symbolTable ast
            let tac = tacGenerate st ast
            nasmGenerate tac st `shouldBe` NASMProgram [] [LABEL "fibonacci",PUSH1 BP,MOV1 DWORD BP SP,PUSH1 B,PUSH1 SI,PUSH1 DI,MOV2 (Register A DWORD) (AddressRegisterOffset (Register BP DWORD) 8 1),CMP4 (Register A DWORD) 0,JL "l1",JMP "l2",LABEL "l1",MOV4 (Register A DWORD) 1,NEG1 (Register A DWORD),POP1 DI,POP1 SI,POP1 B,MOV1 DWORD SP BP,POP1 BP,RET,JMP "l3",LABEL "l2",CMP4 (Register A DWORD) 1,JL "l4",JMP "l5",LABEL "l4",MOV4 (Register A DWORD) 0,POP1 DI,POP1 SI,POP1 B,MOV1 DWORD SP BP,POP1 BP,RET,JMP "l6",LABEL "l5",CMP4 (Register A DWORD) 2,JL "l7",JMP "l8",LABEL "l7",MOV4 (Register A DWORD) 1,POP1 DI,POP1 SI,POP1 B,MOV1 DWORD SP BP,POP1 BP,RET,JMP "l9",LABEL "l8",MOV1 DWORD C A,SUB4 (Register C DWORD) 1,PUSH1 A,PUSH1 D,PUSH1 C,CALL "fibonacci",ADD4 (Register SP DWORD) 4,MOV1 DWORD C A,POP1 D,POP1 A,SUB4 (Register A DWORD) 2,PUSH1 C,PUSH1 D,PUSH1 A,CALL "fibonacci",ADD4 (Register SP DWORD) 4,MOV1 DWORD A A,POP1 D,POP1 C,ADD1 DWORD A C,POP1 DI,POP1 SI,POP1 B,MOV1 DWORD SP BP,POP1 BP,RET,LABEL "l9",LABEL "l6",LABEL "l3",POP1 DI,POP1 SI,POP1 B,MOV1 DWORD SP BP,POP1 BP,RET,LABEL "tiny",PUSH1 BP,MOV1 DWORD BP SP,MOV4 (Register A DWORD) 0,LABEL "l10",CMP4 (Register A DWORD) 10,JL "l12",JMP "l11",LABEL "l12",PUSH1 A,PUSH1 D,PUSH1 A,CALL "fibonacci",ADD4 (Register SP DWORD) 4,MOV1 DWORD C A,POP1 D,POP1 A,PUSH1 A,PUSH1 C,PUSH1 D,PUSH1 C,CALL "_writeint",ADD4 (Register SP DWORD) 4,POP1 D,POP1 C,POP1 A,ADD4 (Register A DWORD) 1,JMP "l10",LABEL "l11",CALL "_exit"]
        it "Generates code for quicksort.c" $ do
            code <- readFile "test/fixtures/quicksort.c"
            let ast = scan_parse_check code
            let st = symbolTable ast
            let tac = tacGenerate st ast
            nasmGenerate tac st `shouldBe` NASMProgram [] [LABEL "quicksort",PUSH1 BP,MOV1 DWORD BP SP,SUB4 (Register SP DWORD) 12,PUSH1 B,PUSH1 SI,PUSH1 DI,MOV2 (Register A DWORD) (AddressRegisterOffset (Register BP DWORD) 8 1),MOV2 (Register D DWORD) (AddressRegisterOffset (Register BP DWORD) 12 1),MOV2 (Register B DWORD) (AddressRegisterOffset (Register BP DWORD) 16 1),MOV1 DWORD C B,SUB1 DWORD C D,CMP4 (Register C DWORD) 0,JG "l1",JMP "l2",LABEL "l1",MOV2 (Register B DWORD) (AddressRegisterOffset (Register BP DWORD) 16 1),MOV1 DWORD C B,ADD1 DWORD C D,PUSH1 D,XOR1 DWORD D D,PUSH1 A,MOV1 DWORD A C,PUSH1 SI,MOV4 (Register SI DWORD) 2,IDIV1 SI,POP1 SI,MOV1 DWORD C A,POP1 A,POP1 D,MOV1 DWORD B C,MOV3 (AddressRegisterOffset (Register BP DWORD) (-12) 1) (Register B DWORD),MOV2 (Register B DWORD) (AddressRegisterOffset (Register BP DWORD) (-12) 1),MOV2 (Register C DWORD) (AddressRegisterRegister (Register A DWORD) 0 (Register B DWORD) 4),MOV1 DWORD B C,MOV3 (AddressRegisterOffset (Register BP DWORD) (-8) 1) (Register B DWORD),MOV1 DWORD C D,MOV2 (Register B DWORD) (AddressRegisterOffset (Register BP DWORD) 16 1),MOV1 DWORD SI B,LABEL "l3",CMP1 (Register C DWORD) (Register SI DWORD),JL "l5",JMP "l4",LABEL "l5",MOV4 (Register B DWORD) 0,MOV3 (AddressRegisterOffset (Register BP DWORD) (-4) 1) (Register B DWORD),LABEL "l6",MOV2 (Register DI DWORD) (AddressRegisterRegister (Register A DWORD) 0 (Register C DWORD) 4),MOV2 (Register B DWORD) (AddressRegisterOffset (Register BP DWORD) (-8) 1),CMP1 (Register DI DWORD) (Register B DWORD),JL "l8",JMP "l7",LABEL "l8",ADD4 (Register C DWORD) 1,JMP "l6",LABEL "l7",LABEL "l9",MOV2 (Register DI DWORD) (AddressRegisterRegister (Register A DWORD) 0 (Register SI DWORD) 4),MOV2 (Register B DWORD) (AddressRegisterOffset (Register BP DWORD) (-8) 1),CMP1 (Register DI DWORD) (Register B DWORD),JG "l11",JMP "l10",LABEL "l11",SUB4 (Register SI DWORD) 1,JMP "l9",LABEL "l10",CMP1 (Register C DWORD) (Register SI DWORD),JL "l12",JMP "l13",LABEL "l12",MOV2 (Register DI DWORD) (AddressRegisterRegister (Register A DWORD) 0 (Register C DWORD) 4),MOV1 DWORD B DI,MOV3 (AddressRegisterOffset (Register BP DWORD) (-4) 1) (Register B DWORD),MOV2 (Register DI DWORD) (AddressRegisterRegister (Register A DWORD) 0 (Register SI DWORD) 4),MOV3 (AddressRegisterRegister (Register A DWORD) 0 (Register C DWORD) 4) (Register DI DWORD),MOV2 (Register B DWORD) (AddressRegisterOffset (Register BP DWORD) (-4) 1),MOV3 (AddressRegisterRegister (Register A DWORD) 0 (Register SI DWORD) 4) (Register B DWORD),ADD4 (Register C DWORD) 1,SUB4 (Register SI DWORD) 1,JMP "l14",LABEL "l13",CMP1 (Register C DWORD) (Register SI DWORD),JE "l15",JMP "l16",LABEL "l15",MOV2 (Register DI DWORD) (AddressRegisterRegister (Register A DWORD) 0 (Register C DWORD) 4),MOV1 DWORD B DI,MOV3 (AddressRegisterOffset (Register BP DWORD) (-4) 1) (Register B DWORD),MOV2 (Register DI DWORD) (AddressRegisterRegister (Register A DWORD) 0 (Register SI DWORD) 4),MOV3 (AddressRegisterRegister (Register A DWORD) 0 (Register C DWORD) 4) (Register DI DWORD),MOV2 (Register B DWORD) (AddressRegisterOffset (Register BP DWORD) (-4) 1),MOV3 (AddressRegisterRegister (Register A DWORD) 0 (Register SI DWORD) 4) (Register B DWORD),ADD4 (Register C DWORD) 1,SUB4 (Register SI DWORD) 1,LABEL "l16",LABEL "l14",JMP "l3",LABEL "l4",MOV2 (Register B DWORD) (AddressRegisterOffset (Register BP DWORD) (-12) 1),PUSH1 A,PUSH1 C,PUSH1 D,PUSH1 B,PUSH1 D,PUSH1 A,CALL "quicksort",ADD4 (Register SP DWORD) 12,POP1 D,POP1 C,POP1 A,MOV2 (Register B DWORD) (AddressRegisterOffset (Register BP DWORD) (-12) 1),MOV1 DWORD C B,ADD4 (Register C DWORD) 1,MOV2 (Register B DWORD) (AddressRegisterOffset (Register BP DWORD) 16 1),PUSH1 A,PUSH1 C,PUSH1 D,PUSH1 B,PUSH1 C,PUSH1 A,CALL "quicksort",ADD4 (Register SP DWORD) 12,POP1 D,POP1 C,POP1 A,LABEL "l2",POP1 DI,POP1 SI,POP1 B,MOV1 DWORD SP BP,POP1 BP,RET,LABEL "tiny",PUSH1 BP,MOV1 DWORD BP SP,SUB4 (Register SP DWORD) 20,MOV1 DWORD A SP,MOV4 (Register C DWORD) 0,MOV5 DWORDADDRESS (AddressRegisterOffset (Register A DWORD) 0 4) 5,MOV5 DWORDADDRESS (AddressRegisterOffset (Register A DWORD) 1 4) 2,MOV5 DWORDADDRESS (AddressRegisterOffset (Register A DWORD) 2 4) 3,MOV5 DWORDADDRESS (AddressRegisterOffset (Register A DWORD) 3 4) 1,MOV5 DWORDADDRESS (AddressRegisterOffset (Register A DWORD) 4 4) 4,LABEL "l17",CMP4 (Register C DWORD) 5,JL "l19",JMP "l18",LABEL "l19",MOV2 (Register D DWORD) (AddressRegisterRegister (Register A DWORD) 0 (Register C DWORD) 4),PUSH1 A,PUSH1 C,PUSH1 D,PUSH1 D,CALL "_writeint",ADD4 (Register SP DWORD) 4,POP1 D,POP1 C,POP1 A,ADD4 (Register C DWORD) 1,JMP "l17",LABEL "l18",MOV4 (Register C DWORD) 0,PUSH1 A,PUSH1 C,PUSH1 D,PUSH3 4,PUSH3 0,PUSH1 A,CALL "quicksort",ADD4 (Register SP DWORD) 12,POP1 D,POP1 C,POP1 A,LABEL "l20",CMP4 (Register C DWORD) 5,JL "l22",JMP "l21",LABEL "l22",MOV2 (Register D DWORD) (AddressRegisterRegister (Register A DWORD) 0 (Register C DWORD) 4),PUSH1 A,PUSH1 C,PUSH1 D,PUSH1 D,CALL "_writeint",ADD4 (Register SP DWORD) 4,POP1 D,POP1 C,POP1 A,ADD4 (Register C DWORD) 1,JMP "l20",LABEL "l21",CALL "_exit"]
        it "Generates code for testchar.c" $ do
            code <- readFile "test/fixtures/testchar.c"
            let ast = scan_parse_check code
            let st = symbolTable ast
            let tac = tacGenerate st ast
            let nasm = nasmGenerate tac st
            nasm `shouldBe` NASMProgram [NASMData "c" DB [104]] [LABEL "tiny",PUSH1 BP,MOV1 DWORD BP SP,MOV4 (Register C LSB) 101,MOV2 (Register A DWORD) (AddressLabelOffset "c" 0 1),ADD4 (Register A DWORD) 5,PUSH1 A,PUSH1 C,PUSH1 D,PUSH1 A,CALL "_writeint",ADD4 (Register SP DWORD) 4,POP1 D,POP1 C,POP1 A,PUSH1 A,PUSH1 C,PUSH1 D,PUSH1 C,CALL "_writechar",ADD4 (Register SP DWORD) 4,POP1 D,POP1 C,POP1 A,CALL "_exit"]
        it "Generates code for pointers.c" $ do
            code <- readFile "test/fixtures/pointers.c"
            let ast = scan_parse_check code
            let st = symbolTable ast
            let tac = tacGenerate st ast
            let nasm = nasmGenerate tac st
            nasm `shouldBe` NASMProgram [] [LABEL "tiny",PUSH1 BP,MOV1 DWORD BP SP,SUB4 (Register SP DWORD) 4,MOV4 (Register B DWORD) 5,MOV3 (AddressRegisterOffset (Register BP DWORD) (-4) 1) (Register B DWORD),MOV4 (Register A DWORD) 0,SUB4 (Register SP DWORD) 20,MOV1 DWORD D SP,MOV5 DWORDADDRESS (AddressRegisterOffset (Register D DWORD) 0 4) 10,MOV5 DWORDADDRESS (AddressRegisterOffset (Register D DWORD) 1 4) 15,MOV5 DWORDADDRESS (AddressRegisterOffset (Register D DWORD) 2 4) 20,LEA A (AddressRegisterOffset (Register D DWORD) 2 4),MOV2 (Register C DWORD) (AddressRegisterOffset (Register A DWORD) 0 1),PUSH1 A,PUSH1 C,PUSH1 D,PUSH1 C,CALL "_writeint",ADD4 (Register SP DWORD) 4,POP1 D,POP1 C,POP1 A,MOV2 (Register B DWORD) (AddressRegisterOffset (Register BP DWORD) (-4) 1),LEA A (AddressRegisterOffset (Register BP DWORD) (-4) 1),MOV2 (Register A DWORD) (AddressRegisterOffset (Register A DWORD) 0 1),PUSH1 A,PUSH1 C,PUSH1 D,PUSH1 A,CALL "_writeint",ADD4 (Register SP DWORD) 4,POP1 D,POP1 C,POP1 A,MOV1 DWORD A D,MOV2 (Register C DWORD) (AddressRegisterOffset (Register A DWORD) 0 1),PUSH1 A,PUSH1 C,PUSH1 D,PUSH1 C,CALL "_writeint",ADD4 (Register SP DWORD) 4,POP1 D,POP1 C,POP1 A,MOV2 (Register A DWORD) (AddressRegisterOffset (Register A DWORD) 1 4),PUSH1 A,PUSH1 C,PUSH1 D,PUSH1 A,CALL "_writeint",ADD4 (Register SP DWORD) 4,POP1 D,POP1 C,POP1 A,MOV2 (Register A DWORD) (AddressRegisterOffset (Register D DWORD) 0 1),PUSH1 A,PUSH1 C,PUSH1 D,PUSH1 A,CALL "_writeint",ADD4 (Register SP DWORD) 4,POP1 D,POP1 C,POP1 A,CALL "_exit"]
        it "Generates code for pointers2.c" $ do
            code <- readFile "test/fixtures/pointers2.c"
            let ast = scan_parse_check code
            let st = symbolTable ast
            let tac = tacGenerate st ast
            let nasm = nasmGenerate tac st
            nasm `shouldBe` NASMProgram [] []
        it "Generates code for quicksort2.c" $ do
            code <- readFile "test/fixtures/quicksort2.c"
            let ast = scan_parse_check code
            let st = symbolTable ast
            let tac = tacGenerate st ast
            let (vMap, spilled, is) = mapVariablesToRegisters (concat $ tacCode tac) 6 M.empty
            nasmGenerate tac st `shouldBe` NASMProgram [] []