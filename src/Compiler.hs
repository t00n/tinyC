module Compiler (compile, run_parse, run_st, run_semantics, run_tac, run_nasm) where

import Control.Arrow ((&&&), second)
import qualified Data.Map as M
import Control.Monad (liftM2)

import Scanner
import Parser
import AST
import SemanticError
import SymbolTable
import Semantics
import TACGenerator
import TACProgram
import TACAnalysis
import NASMGenerator
import Utility

-- API
run_parse :: String -> Program
run_parse = parse . scan

run_st :: Program -> Either SemanticError SymbolTable
run_st = runST

run_semantics :: Program -> Either SemanticError Program
run_semantics prog = run_st_ast prog >>= uncurry runSemantics

run_tac :: String -> TACProgram
run_tac = uncurry tacGenerate . st_ast_check . run_parse

run_nasm :: String -> NASMProgram
run_nasm = uncurry nasmGenerate . (eval_st &&& run_tac)

-- Helpers

eval_st :: String -> SymbolTable
eval_st = evalST . run_parse

st_ast :: Program -> (SymbolTable, Program)
st_ast = evalST &&& id

run_st_ast :: Program -> Either SemanticError (SymbolTable, Program)
run_st_ast prog = runST prog >>= \st -> return (st, prog)

st_ast_check :: Program -> (SymbolTable, Program)
st_ast_check = (fst &&& uncurry evalSemantics) . st_ast

compile :: String -> String -> IO ()
compile infile outfile = 
    readFile infile >>= writeFile outfile . nasmShow . run_nasm