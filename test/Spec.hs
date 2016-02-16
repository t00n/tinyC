import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

main :: IO ()
main = hspec $ do
    describe "First test" $ do
        it "does someting" $ do
            5 `shouldBe` (5 :: Int)

