import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate, ErrorCall(..))
import System.IO

import qualified Data.Tree as T
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe (fromJust)

import Scanner
import Parser
import Semantics
import TACGenerator
import MonadNames
import NASMGenerator
import SymbolTable
import SemanticError
import TACAnalysis
import Graph
import NASMAnalysis

import TestScanner
import TestParser
import TestSymbolTable
import TestSemantics
import TestTACGenerator
import TestTACAnalysis
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
    testNASMGenerator
    describe "Tests nasm analysis and optimization" $ do
        it "Tests negative constraints on register allocation" $ do
            code <- readFile "test/fixtures/bigprogram.c"
            let ast = scan_parse_check code
            let st = symbolTable ast
            let tac = tacGenerate st ast
            negativeConstraints (concat $ tacCode tac) `shouldBe` M.fromList [("t2",S.fromList [A,D]),("t6",S.fromList [A,D]),("t9",S.fromList [A,D])]
    testRealLife
