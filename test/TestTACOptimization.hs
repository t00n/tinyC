module TestTACOptimization (testTACOptimization) where

import Test.Hspec

import Compiler
import AST
import TACProgram
import TACOptimization

testTACOptimization =
    describe "Tests TAC Optimization" $ do
        it "Tests tac optimization on pointers.c" $ do
            code <- readFile "test/fixtures/pointers.c"
            let tac = run_tac code
            let optimizedTAC = tacOptimize tac
            optimizedTAC `shouldBe` TACProgram [] [[TACLabel "tiny",TACCopy "a" (TACInt 0),TACCopy "x" (TACInt 5),TACCopy "b" (TACInt 0),TACArrayDecl "xs" [TACInt 0,TACInt 0,TACInt 0,TACInt 0,TACInt 0],TACArrayModif "xs" (TACInt 0) (TACInt 10),TACArrayModif "xs" (TACInt 1) (TACInt 15),TACArrayModif "xs" (TACInt 2) (TACInt 20),TACAddress "b" (TACArray "xs" (TACInt 2)),TACDeRef "t2" (TACVar "b"),TACWrite IntType (TACVar "t2"),TACCopy "a" (TACVar "b"),TACDeRefA "a" (TACInt 5),TACAddress "b" (TACVar "x"),TACDeRef "t4" (TACVar "b"),TACWrite IntType (TACVar "t4"),TACCopy "b" (TACVar "xs"),TACDeRef "t5" (TACVar "b"),TACWrite IntType (TACVar "t5"),TACArrayAccess "t6" "b" (TACInt 1),TACWrite IntType (TACVar "t6"),TACDeRef "t7" (TACVar "xs"),TACWrite IntType (TACVar "t7"),TACReturn Nothing]]
        it "Tests that unused global variables are removed" $ do
            let tac = run_tac "int a; int b = 5; int tiny() { }"
            let optimizedTAC = tacOptimize tac
            optimizedTAC `shouldBe` TACProgram [] [[TACLabel "tiny", TACReturn Nothing]]