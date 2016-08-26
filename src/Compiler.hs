module Compiler (compile) where

import Scanner
import Parser
import Semantics
import TACGenerator
import NASMGenerator

compile :: String -> String -> IO ()
compile infile outfile = do
    cCode <- readFile infile
    let ast = case (checkSemantics . parse . alexScanTokens) cCode of
                   Left x -> error $ show x
                   Right x -> x
    let st = symbolTable ast
    let tac = tacGenerate st ast
    writeFile (outfile ++ ".tac") $ tacPrint tac
    let nasm = nasmGenerate tac st
    writeFile outfile $ nasmShow nasm