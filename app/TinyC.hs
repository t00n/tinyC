module Main where

import System.Environment

import Compiler

main :: IO ()
main = do
    args <- getArgs
    let infile = args !! 0
    let outfile = args !! 1
    compile infile outfile