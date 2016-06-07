module Main where

import System.Environment

import Scanner
import Parser
import Semantics
import TACGenerator
import NASMGenerator

main :: IO ()
main = do
    args <- getArgs
    let infile = args !! 0
    let outfile = args !! 1
    cCode <- readFile infile
    let ast = (parse . alexScanTokens) cCode
    let st = symbolTable ast
    let tac = tacGenerate ast
    let nasm = nasmGenerate tac st
    writeFile outfile $ nasmShow nasm