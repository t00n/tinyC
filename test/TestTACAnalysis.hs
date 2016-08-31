module TestTACAnalysis (testTACAnalysis) where

import Test.Hspec
import qualified Data.Map as M
import qualified Data.Set as S

import Compiler
import Graph
import TACGenerator
import TACProgram
import TACAnalysis

testTACAnalysis = 
	describe "Tests live variable analysis" $ do
        it "tests label key construction" $ do
            let tac = run_tac "int tiny() { if(5) { 5; } else { 3; } } int f() {}"
            constructLabelKey ((tacCode tac) !! 0) `shouldBe` M.fromList [("l1",3),("l2",5),("l3",6),("tiny",0)]
        it "tests graph creation" $ do
            let tac = run_tac "int a; int tiny() { int d = 5; if (a) { int e = 1 + d; } else { int c = 3; } } int f() { int c = 3 + 4; return c; } int b = 3;"
            let cfg = controlFlowGraph (concat (tacCode tac))
            let dfg = dataFlowGraph cfg
            let rig = registerInterferenceGraph dfg
            cfg `shouldBe` Graph (M.fromList [(0,TACLabel "tiny"),(1,TACCopy "d" (TACInt 5)),(2,TACIf (TACExpr (TACVar "a") TACNotEqual (TACInt 0)) "l1"),(3,TACGoto "l2"),(4,TACLabel "l1"),(5,TACBinary "t1" (TACInt 1) TACPlus (TACVar "d")),(6,TACCopy "e" (TACVar "t1")),(7,TACGoto "l3"),(8,TACLabel "l2"),(9,TACCopy "c" (TACInt 3)),(10,TACLabel "l3"),(11,TACReturn Nothing),(12,TACLabel "f"),(13,TACBinary "t2" (TACInt 3) TACPlus (TACInt 4)),(14,TACCopy "c" (TACVar "t2")),(15,TACReturn (Just (TACVar "c")))]) (S.fromList [(0,1),(1,2),(2,3),(2,4),(3,8),(4,5),(5,6),(6,7),(7,10),(8,9),(9,10),(10,11),(12,13),(13,14),(14,15)])
            dfg `shouldBe` M.fromList [(0,(S.fromList ["a"],S.fromList ["a"])),(1,(S.fromList ["a"],S.fromList ["a","d"])),(2,(S.fromList ["a","d"],S.fromList ["d"])),(3,(S.fromList [],S.fromList [])),(4,(S.fromList ["d"],S.fromList ["d"])),(5,(S.fromList ["d"],S.fromList ["t1"])),(6,(S.fromList ["t1"],S.fromList [])),(7,(S.fromList [],S.fromList [])),(8,(S.fromList [],S.fromList [])),(9,(S.fromList [],S.fromList [])),(10,(S.fromList [],S.fromList [])),(11,(S.fromList [],S.fromList [])),(12,(S.fromList [],S.fromList [])),(13,(S.fromList [],S.fromList ["t2"])),(14,(S.fromList ["t2"],S.fromList ["c"])),(15,(S.fromList ["c"],S.fromList []))]
            rig `shouldBe` Graph (M.fromList [(0,"a"),(1,"c"),(2,"d"),(3,"t1"),(4,"t2")]) (S.fromList [(0,2),(2,0)])