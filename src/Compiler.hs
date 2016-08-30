module Compiler (compile) where

import Scanner
import Parser
import Semantics
import TACGenerator
import TACAnalysis
import NASMGenerator

import qualified Data.Map as M

compile :: String -> String -> IO ()
compile infile outfile = do
    cCode <- readFile infile
    let ast = case (checkSemantics . parse . alexScanTokens) cCode of
                   Left x -> error $ show x
                   Right x -> x
    let st = symbolTable ast
    let tac = tacGenerate st ast
    let newtac = concatMap (\x -> let (a, b, c) = mapVariablesToRegisters x 6 M.empty in c) (tacCode tac)
    writeFile (outfile ++ ".tac") $ tacPrint newtac
    let nasm = nasmGenerate tac st
    writeFile outfile $ nasmShow nasm