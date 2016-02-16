import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import System.IO

import Scanner

main :: IO ()
--main = hspec $ do
--    describe "First test" $ do
--        it "does someting" $ do
--            5 `shouldBe` (5 :: Int)
main = do
    s <- readFile "fixtures/trivial.c"
    tokens <- alexScanTokens s
    putStrLn tokens

