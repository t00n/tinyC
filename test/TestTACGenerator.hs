module TestTACGenerator (testTACGenerator) where

import Test.Hspec

import Compiler
import AST (Type(..))
import TACGenerator

testTACGenerator = 
	describe "The generation of three-address-code" $ do
        it "Generates a few declarations" $ do
            let tac = run_tac "int a; int b; int tiny() {}"
            tac `shouldBe` TACProgram [TACCopy "a" (TACInt 0),TACCopy "b" (TACInt 0)] [[TACLabel "tiny",TACReturn Nothing]]
        it "Generates declarations with complex binary expressions" $ do
            let tac = run_tac "int a = 5; int tiny() { int b = (a+5)/(a-2); }"
            tac `shouldBe` TACProgram [TACCopy "a" (TACInt 5)] [[TACLabel "tiny",TACBinary "t1" (TACVar "a") TACPlus (TACInt 5),TACBinary "t2" (TACVar "a") TACMinus (TACInt 2),TACBinary "t3" (TACVar "t1") TACDivide (TACVar "t2"),TACCopy "b" (TACVar "t3"),TACReturn Nothing]]
        it "Generates function declarations" $ do
            let tac = run_tac "int tiny() {}"
            tac `shouldBe` TACProgram [] [[TACLabel "tiny",TACReturn Nothing]]
            let tac = run_tac "int f(int a, int b) {int c = 4;} int tiny() { }"
            tac `shouldBe` TACProgram [] [[TACLabel "f",TACCopy "c" (TACInt 4),TACReturn Nothing],[TACLabel "tiny",TACReturn Nothing]]
        it "Generates declarations with complex unary expressions" $ do
            let tac = run_tac "int a = 5; int tiny() { int b = -(a - 5); }"
            tac `shouldBe` TACProgram [TACCopy "a" (TACInt 5)] [[TACLabel "tiny",TACBinary "t1" (TACVar "a") TACMinus (TACInt 5),TACUnary "t2" TACNeg (TACVar "t1"),TACCopy "b" (TACVar "t2"),TACReturn Nothing]]
        it "Generates function calls" $ do
            let tac = run_tac "int f(int a, int b) { int c = 1; f(c, 2); } int tiny() {}"
            tac `shouldBe` TACProgram [] [[TACLabel "f",TACCopy "c" (TACInt 1),TACCall "f" [TACVar "c",TACInt 2] Nothing,TACReturn Nothing],[TACLabel "tiny",TACReturn Nothing]]
            let tac = run_tac "int f(int a, int b) { int c = f(2 + 3, 1); } int tiny() {}"
            tac `shouldBe` TACProgram [] [[TACLabel "f",TACBinary "t1" (TACInt 2) TACPlus (TACInt 3),TACCall "f" [TACVar "t1",TACInt 1] (Just $ TACVar "t2"),TACCopy "c" (TACVar "t2"),TACReturn Nothing],[TACLabel "tiny",TACReturn Nothing]]
        it "Generates assignments" $ do
            let tac = run_tac "int tiny() { int a; a = (a + 5) * 3; }"
            tac `shouldBe` TACProgram [] [[TACLabel "tiny",TACCopy "a" (TACInt 0),TACBinary "t1" (TACVar "a") TACPlus (TACInt 5),TACBinary "t2" (TACVar "t1") TACTimes (TACInt 3),TACCopy "a" (TACVar "t2"),TACReturn Nothing]]
            let tac = run_tac "int a[5]; int b = a[2]; int tiny() {}"
            tac `shouldBe` TACProgram [TACArrayDecl "a" [TACInt 0,TACInt 0,TACInt 0,TACInt 0,TACInt 0],TACArrayAccess "t1" "a" (TACInt 2),TACCopy "b" (TACVar "t1")] [[TACLabel "tiny",TACReturn Nothing]]
            let tac = run_tac "int a[5]; int tiny() { a[2] = 5; }"
            tac `shouldBe` TACProgram [TACArrayDecl "a" [TACInt 0,TACInt 0,TACInt 0,TACInt 0,TACInt 0]] [[TACLabel "tiny",TACArrayModif "a" (TACInt 2) (TACInt 5),TACReturn Nothing]]
            let tac = run_tac "int a[5]; int b[5]; int tiny() { a[2] = b[3]; }"
            tac `shouldBe` TACProgram [TACArrayDecl "a" [TACInt 0,TACInt 0,TACInt 0,TACInt 0,TACInt 0],TACArrayDecl "b" [TACInt 0,TACInt 0,TACInt 0,TACInt 0,TACInt 0]] [[TACLabel "tiny",TACArrayAccess "t1" "b" (TACInt 3),TACArrayModif "a" (TACInt 2) (TACVar "t1"),TACReturn Nothing]]
        it "Generates if" $ do
            let tac = run_tac "int tiny() { if (1 > 2) { int a = 5; a = 3; } }"
            tac `shouldBe` TACProgram [] [[TACLabel "tiny",TACIf (TACExpr (TACInt 1) TACGreater (TACInt 2)) "l1",TACGoto "l2",TACLabel "l1",TACCopy "a" (TACInt 5),TACCopy "a" (TACInt 3),TACLabel "l2",TACReturn Nothing]]
        it "Generates if else" $ do
            let tac = run_tac "int tiny() { if (1 > 2) { int a = 5; } else { int b = 5; } }"
            tac `shouldBe` TACProgram [] [[TACLabel "tiny",TACIf (TACExpr (TACInt 1) TACGreater (TACInt 2)) "l1",TACGoto "l2",TACLabel "l1",TACCopy "a" (TACInt 5),TACGoto "l3",TACLabel "l2",TACCopy "b" (TACInt 5),TACLabel "l3",TACReturn Nothing]]
        it "Generates several if else" $ do
            let tac = run_tac "int tiny() { if (1 > 2) { int a = 1; } else if (2 > 3) { int b = 2; } else { int c = 3; } }"
            tac `shouldBe` TACProgram [] [[TACLabel "tiny",TACIf (TACExpr (TACInt 1) TACGreater (TACInt 2)) "l1",TACGoto "l2",TACLabel "l1",TACCopy "a" (TACInt 1),TACGoto "l3",TACLabel "l2",TACIf (TACExpr (TACInt 2) TACGreater (TACInt 3)) "l4",TACGoto "l5",TACLabel "l4",TACCopy "b" (TACInt 2),TACGoto "l6",TACLabel "l5",TACCopy "c" (TACInt 3),TACLabel "l6",TACLabel "l3",TACReturn Nothing]]
        it "Generates a while" $ do
            let tac = run_tac "int tiny() { int a = 2; while ( a > 1) { a = a - 1; } }"
            tac `shouldBe` TACProgram [] [[TACLabel "tiny",TACCopy "a" (TACInt 2),TACLabel "l1",TACIf (TACExpr (TACVar "a") TACGreater (TACInt 1)) "l3",TACGoto "l2",TACLabel "l3",TACBinary "t1" (TACVar "a") TACMinus (TACInt 1),TACCopy "a" (TACVar "t1"),TACGoto "l1",TACLabel "l2",TACReturn Nothing]]
        it "Generates a return" $ do
            let tac = run_tac "int tiny() { int a = 2; return a; }"
            tac `shouldBe` TACProgram [] [[TACLabel "tiny",TACCopy "a" (TACInt 2),TACReturn (Just (TACVar "a"))]]
            let tac = run_tac "int tiny() { return 3 + 4 / 5; }"
            tac `shouldBe` TACProgram [] [[TACLabel "tiny",TACBinary "t1" (TACInt 4) TACDivide (TACInt 5),TACBinary "t2" (TACInt 3) TACPlus (TACVar "t1"),TACReturn (Just (TACVar "t2"))]]
        it "Generates a write" $ do
            let tac = run_tac "int tiny() { int a = 2; write a; }"
            tac `shouldBe` TACProgram [] [[TACLabel "tiny",TACCopy "a" (TACInt 2),TACWrite IntType (TACVar "a"),TACReturn Nothing]]
            let tac = run_tac "int tiny() { int a[5]; write a[2]; }"
            tac `shouldBe` TACProgram [] [[TACLabel "tiny",TACArrayDecl "a" [TACInt 0,TACInt 0,TACInt 0,TACInt 0,TACInt 0],TACArrayAccess "t1" "a" (TACInt 2),TACWrite IntType (TACVar "t1"),TACReturn Nothing]]
        it "Generates reads" $ do
            let tac = run_tac "int tiny() { int a; read a; }"
            tac `shouldBe` TACProgram [] [[TACLabel "tiny",TACCopy "a" (TACInt 0),TACRead IntType (TACVar "a"),TACReturn Nothing]]
            let tac = run_tac "int tiny() { int a[5]; read a[2]; }"
            tac `shouldBe` TACProgram [] [[TACLabel "tiny",TACArrayDecl "a" [TACInt 0,TACInt 0,TACInt 0,TACInt 0,TACInt 0],TACArrayAccess "t1" "a" (TACInt 2),TACRead IntType (TACVar "t1"),TACReturn Nothing]]