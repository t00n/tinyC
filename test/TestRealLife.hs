module TestRealLife (testRealLife) where

import Test.Hspec
import System.Process
import System.IO

import Compiler

testCompile infile result = testCompileWithInput infile [] result

testCompileWithInput infile args result = do
    compile ("test/fixtures/" ++ infile) "test.s"
    readProcess "nasm" ["-felf32", "-g", "test.s"] []
    readProcess "ld" ["-melf_i386", "-e", "tiny", "-g", "test.o", "-o", "test.app"] []
    content <- readProcess "./test.app" [] args
    content `shouldBe` result


testRealLife = 
    describe "Tests the all chain from C to binary on real life examples" $ do
        it "Tests the ackermann function" $ do
            testCompile "ackermann.c" "123234357"
        it "Tests the big program" $ do
            testCompile "bigprogram.c" "-20"
        it "Tests an empty program" $ do
            testCompile "empty.c" ""
        it "Tests the modulo and two euclides algorithm" $ do
            testCompile "euclides.c" "261416141"
        it "Tests the recursive fibonacci function" $ do
            testCompile "fibonacci.c" "0112358132134"
        it "Tests a program with local pointers" $ do
            testCompile "pointers.c" "205101510"
        it "Tests a program with global pointers" $ do
            testCompile "pointers2.c" "205101510"
        it "Tests quicksort with arrays" $ do
            testCompile "quicksort.c" "5231412345"
        it "Tests quicksort with pointers arithmetics" $ do
            testCompile "quicksort2.c" "5231412345"
        it "Tests read and write chars and ints" $ do
            testCompileWithInput "read_and_write.c" "-4587\n" "-4587c"
        it "Tests writing chars" $ do
            testCompile "testchar.c" "109e"
