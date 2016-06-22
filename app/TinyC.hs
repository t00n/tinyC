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
    let ast = case (checkSemantics . parse . alexScanTokens) cCode of
                   Left x -> error $ show x
                   Right x -> x
    let st = symbolTable ast
    let tac = tacGenerate st ast
    let nasm = nasmGenerate tac st
    writeFile outfile $ nasmShow nasm