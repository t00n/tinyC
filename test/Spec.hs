import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate, ErrorCall(..))
import System.IO

import qualified Data.Tree as T
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe (fromJust)

import MonadNames
import TACGenerator
import NASMGenerator
import NASMAnalysis
import Compiler

import TestScanner
import TestParser
import TestSymbolTable
import TestSemantics
import TestTACGenerator
import TestTACAnalysis
import TestTACOptimization
import TestNASMGenerator
import TestRealLife

main :: IO ()
main = hspec $ do
    testTokens
    testParsing
    testSymbolTable
    testSemantics
    testTACGenerator
    describe "Do the name generator works ????" $ do
        it "Tests everything" $ do
            evalNames (do { s1 <- popVariable; s2 <- nextVariable; l1 <- nextLabel; return [s1, s2, l1] }) ["t" ++ show i | i <- [1..]] ["l" ++ show i | i <- [1..]] `shouldBe` ["t1", "t2", "l1"]
    testTACAnalysis
    testTACOptimization
    testNASMGenerator
    describe "Tests nasm analysis and optimization" $ do
        it "Tests negative constraints on register allocation" $ do
            code <- readFile "test/fixtures/bigprogram.c"
            let tac = run_tac code
            negativeConstraints (concat $ tacCode tac) `shouldBe` M.fromList [("t2",S.fromList [A,D]),("t6",S.fromList [A,D]),("t9",S.fromList [A,D])]
    testRealLife
