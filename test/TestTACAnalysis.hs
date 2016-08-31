module TestTACAnalysis (testTACAnalysis) where

import Test.Hspec
import qualified Data.Map as M
import qualified Data.Set as S

import Compiler
import Graph
import Semantics
import TACGenerator
import TACAnalysis


testRegisterMapping :: RegisterMapping -> RegisterInterferenceGraph -> Bool
testRegisterMapping mapping rig = and $ M.elems $ M.mapWithKey f mapping
    where f var reg = reg `notElem` neighboursReg
                where varKey = find var rig
                      varNeighbours = neighboursValues varKey rig
                      neighboursReg = map ((M.!) mapping) varNeighbours

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
        it "tests register allocation with enough registers" $ do
            let tac = run_tac "int a; int tiny() { int b = 2; int c = 3; int d = (a+b)/(b-c); }"
            let cfg = controlFlowGraph ((concat . tacCode) tac)
            let df = dataFlowGraph cfg
            let rig = registerInterferenceGraph df
            rig `shouldBe` Graph (M.fromList [(0,"a"),(1,"b"),(2,"c"),(3,"t1"),(4,"t2"),(5,"t3")]) (S.fromList [(0,1),(0,2),(1,0),(1,2),(1,3),(2,0),(2,1),(2,3),(3,1),(3,2),(3,4),(4,3)])
            let (nodes, spilled) = simplifyRIG rig 3
            (nodes, spilled) `shouldBe` (["t1","c","b","a","t2","t3"],[])
            let registerMapping = findRegisters nodes rig 3 M.empty
            testRegisterMapping registerMapping rig `shouldBe` True
            registerMapping `shouldBe` M.fromList [("a",0),("b",2),("c",1),("t1",0),("t2",1),("t3",0)]
        it "tests register allocation with enough registers on real program" $ do
            code <- readFile "test/fixtures/fibonacci.c"
            let tac = run_tac code
            let cfg = controlFlowGraph ((concat . tacCode) tac)
            let df = dataFlowGraph cfg
            let rig = registerInterferenceGraph df
            let (nodes, spilled) = simplifyRIG rig 3
            (nodes, spilled) `shouldBe` (["t5","t3","t4","n","t2","t7","i","t8","t6","t1"],[])
            let registerMapping = findRegisters nodes rig 3 M.empty
            testRegisterMapping registerMapping rig `shouldBe` True
            registerMapping `shouldBe` M.fromList [("i",1),("n",0),("t1",0),("t2",1),("t3",1),("t4",0),("t5",0),("t6",0),("t7",0),("t8",0)]
        it "tests register alloction with not enough registers" $ do
            code <- readFile "test/fixtures/bigprogram.c"
            let k = 6
            let tac = run_tac code
            let cfg = controlFlowGraph (concat $ tacCode tac)
            let dfg = dataFlowGraph cfg
            let rig = registerInterferenceGraph dfg
            let (nodes, spilled) = simplifyRIG rig k
            let newtac = fixInstructions (concat $ tacCode tac) spilled
            let cfg = controlFlowGraph newtac
            let dfg = dataFlowGraph cfg
            let rig = registerInterferenceGraph dfg
            let newspilled = spillMoreVariables newtac spilled dfg k
            let newnewtac = fixInstructions (concat $ tacCode tac) newspilled
            let cfg = controlFlowGraph newnewtac
            let dfg = dataFlowGraph cfg
            let rig = registerInterferenceGraph dfg
            let registerMapping = findRegisters nodes rig k M.empty
            testRegisterMapping registerMapping rig `shouldBe` True
            registerMapping `shouldBe` M.fromList [("a",0),("b",5),("c",4),("d",3),("t1",1),("t10",0),("t11",0),("t2",0),("t3",2),("t4",1),("t5",0),("t6",0),("t7",1),("t8",0),("t9",2)]
        it "Tests register allocation on pointers.c" $ do
            code <- readFile "test/fixtures/pointers.c"
            let tac = run_tac code
            let (registerMapping, spilled, newfunc) = mapVariablesToRegisters (tacCode tac !! 0) 6 M.empty
            registerMapping `shouldBe` M.fromList [("a",0),("b",1),("t1",0),("t2",0),("t3",0),("t4",0),("t5",0),("t6",0),("t7",0),("x",0),("xs",0)]