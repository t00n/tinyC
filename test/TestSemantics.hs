module TestSemantics (testSemantics) where

import Test.Hspec

import Compiler
import Semantics
import SemanticError

testSemantics = 
    describe "Semantics" $ do
        it "Checks that variables with same name are declared only once on a certain scope level" $ do
            let ast = run_parse "int tiny() { int a; int a; }"
            run_semantics ast `shouldBe` Left (SemanticError NameExistsError "a")
        it "Checks that variables with same name can be declared more than once a different scope level with a warning" $ do
            let ast = run_parse "int a; int tiny() { int a; }"
            run_semantics ast `shouldBe` Left (SemanticError NameExistsWarning "Name \"a\"")
        it "Checks that variables are declared before use in assignment" $ do
            let ast = run_parse "int tiny() { a = 5; }"
            run_semantics ast `shouldBe` Left (SemanticError NotDeclaredError "Name \"a\"")
            let ast = run_parse "int a = 5; int tiny() { a = 3; }"
            run_semantics ast `shouldBe` Right ast
        it "Checks that variables are declared before use in if" $ do
            let ast = run_parse "int tiny() { if (a) a = 5; }"
            run_semantics ast `shouldBe` Left (SemanticError NotDeclaredError "Name \"a\"")
            let ast = run_parse "int a = 5; int tiny() { int c = 3; if (a) a = 3; }"
            run_semantics ast `shouldBe` Right ast
        it "Checks that variables are declared before use in while" $ do
            let ast = run_parse "int tiny() { while (a) a = 5; }"
            run_semantics ast `shouldBe` Left (SemanticError NotDeclaredError "Name \"a\"")
            let ast = run_parse "int a = 5; int tiny() { while (a) a = 5; }"
            run_semantics ast `shouldBe` Right ast
        it "Checks that variables are declared before use in return" $ do
            let ast = run_parse "int tiny() { return a; }"
            run_semantics ast `shouldBe` Left (SemanticError NotDeclaredError "Name \"a\"")
            let ast = run_parse "int a = 5; int tiny() { return a; }"
            run_semantics ast `shouldBe` Right ast
        it "Checks that variables are declared and scalar before use in IO" $ do
            let ast = run_parse "int tiny() { write a; }"
            run_semantics ast `shouldBe` Left (SemanticError NotDeclaredError "Name \"a\"")
            let ast = run_parse "int a = 5; int tiny() { read a; }"
            run_semantics ast `shouldBe` Right ast
            let ast = run_parse "int a = 5; int tiny() { write a; }"
            run_semantics ast `shouldBe` Right ast
            let ast = run_parse "int a[5]; int tiny() { write a; }"
            run_semantics ast `shouldBe` Left (SemanticError {errorType = NotAValueError, errorVariable = "Var (Name \"a\")"})
            let ast = run_parse "int a[5]; int tiny() { read a; }"
            run_semantics ast `shouldBe` Left (SemanticError {errorType = NotAValueError, errorVariable = "Name \"a\""})
            let ast = run_parse "int a; int tiny() { read a[5]; }"
            run_semantics ast `shouldBe` Left (SemanticError {errorType = NotAnArrayError, errorVariable = "NameSubscription \"a\" [Int 5]"})
            let ast = run_parse "int a[5]; int tiny() { read a[2]; }"
            run_semantics ast `shouldBe` Right ast
        it "Checks that variables are declared before use in expression" $ do
            let ast = run_parse "int tiny() { a; }"
            run_semantics ast `shouldBe` Left (SemanticError NotDeclaredError "Name \"a\"")
            let ast = run_parse "int a = 5; int tiny() { a; }"
            run_semantics ast `shouldBe` Right ast
        it "Checks that variables are declared before use in binary operations" $ do
            let ast = run_parse "int tiny() { a + 5; }"
            run_semantics ast `shouldBe` Left (SemanticError NotDeclaredError "Name \"a\"")
            let ast = run_parse "int a = 5; int tiny() { a + 5; }"
            run_semantics ast `shouldBe` Right ast
            let ast = run_parse "int tiny() { 5 + a; }"
            run_semantics ast `shouldBe` Left (SemanticError NotDeclaredError "Name \"a\"")
            let ast = run_parse "int a = 5; int tiny() { 5 + a; }"
            run_semantics ast `shouldBe` Right ast
        it "Checks that variables are declared before use in unary operations" $ do
            let ast = run_parse "int tiny() { -a; }"
            run_semantics ast `shouldBe` Left (SemanticError NotDeclaredError "Name \"a\"")
            let ast = run_parse "int a = 5; int tiny() { -a; }"
            run_semantics ast `shouldBe` Right ast
        it "Checks that variables are declared before use in function calls and that the variable is a function" $ do
            let ast = run_parse "int tiny() { a(); }"
            run_semantics ast `shouldBe` Left (SemanticError NotDeclaredError "Name \"a\"")
            let ast = run_parse "int a = 5; int tiny() { a(); }"
            run_semantics ast `shouldBe` Left (SemanticError NotAFunctionError "Name \"a\"")
            let ast = run_parse "int tiny() { tiny(); }"
            run_semantics ast `shouldBe` Right ast
        it "Checks that parameters of a function are declared and variables in the scope" $ do
            let ast = run_parse "int tiny() { int a; a + 5; }"
            run_semantics ast `shouldBe` Right ast
            let ast = run_parse "int tiny() { int a; int a; }"
            run_semantics ast `shouldBe` Left (SemanticError {errorType = NameExistsError, errorVariable = "a"})
        it "Checks the arguments of a function call" $ do
            let ast = run_parse "int a = 5; int f(int a, int b) {} int tiny() { f(a, 5); }"
            run_semantics ast `shouldBe` Left (SemanticError {errorType = NameExistsWarning, errorVariable = "Name \"a\""})
            let ast = run_parse "int f(int a1, int b) {} int tiny() { int a; f(a, c); }"
            run_semantics ast `shouldBe` Left (SemanticError {errorType = NotDeclaredError, errorVariable = "Name \"c\""})
            let ast = run_parse "int a[5]; int f(int b) {} int tiny() { f(a); }"
            run_semantics ast `shouldBe` Left (SemanticError {errorType = NotAValueError, errorVariable = "Var (Name \"a\")"})
        it "Checks that variables are declared before use in a length expression and that the variable is an array" $ do
            let ast = run_parse "int tiny() { length a; }"
            run_semantics ast `shouldBe` Left (SemanticError NotDeclaredError "Name \"a\"")
            let ast = run_parse "int a; int tiny() { length a; }"
            run_semantics ast `shouldBe` Left (SemanticError NotAnArrayError "Name \"a\"")
            let ast = run_parse "int a[5]; int tiny() { length a; }"
            run_semantics ast `shouldBe` Right ast
        it "Checks that binary and unary expressions have good kinds" $ do
            let ast = run_parse "int tiny() { int * p; int a[5]; int x; a + p; }"
            run_semantics ast `shouldBe` Left (SemanticError {errorType = NotAValueError, errorVariable = "Var (Name \"p\")"})
            let ast = run_parse "int tiny() { int * p; int a[5]; int x; a + a; }"
            run_semantics ast `shouldBe` Left (SemanticError {errorType = NotAValueError, errorVariable = "Var (Name \"a\")"})
            let ast = run_parse "int tiny() { int * p; int a[5]; int x; a + a[5]; }"
            run_semantics ast `shouldBe` Right ast
            let ast = run_parse "int tiny() { int * p; int a[5]; int x; a + x; }"
            run_semantics ast `shouldBe` Right ast
            let ast = run_parse "int tiny() { int * p; int a[5]; int x; a + *p; }"
            run_semantics ast `shouldBe` Right ast
            let ast = run_parse "int tiny() { int * p; int a[5]; int x; a - p; }"
            run_semantics ast `shouldBe` Right ast
            let ast = run_parse "int tiny() { int * p; int a[5]; int x; a - a; }"
            run_semantics ast `shouldBe` Right ast
            let ast = run_parse "int tiny() { int * p; int a[5]; int x; a - a[5]; }"
            run_semantics ast `shouldBe` Right ast
            let ast = run_parse "int tiny() { int * p; int a[5]; int x; a - x; }"
            run_semantics ast `shouldBe` Right ast
            let ast = run_parse "int tiny() { int * p; int a[5]; int x; a - *p; }"
            run_semantics ast `shouldBe` Right ast
            let ast = run_parse "int tiny() { int * p; int a[5]; int x; a * p; }"
            run_semantics ast `shouldBe` Left (SemanticError {errorType = NotAValueError, errorVariable = "Var (Name \"a\")"})
            let ast = run_parse "int tiny() { int * p; int a[5]; int x; a * a; }"
            run_semantics ast `shouldBe` Left (SemanticError {errorType = NotAValueError, errorVariable = "Var (Name \"a\")"})
            let ast = run_parse "int tiny() { int * p; int a[5]; int x; a * a[5]; }"
            run_semantics ast `shouldBe` Left (SemanticError {errorType = NotAValueError, errorVariable = "Var (Name \"a\")"})
            let ast = run_parse "int tiny() { int * p; int a[5]; int x; a * x; }"
            run_semantics ast `shouldBe` Left (SemanticError {errorType = NotAValueError, errorVariable = "Var (Name \"a\")"})
            let ast = run_parse "int tiny() { int * p; int a[5]; int x; a * *p; }"
            run_semantics ast `shouldBe` Left (SemanticError {errorType = NotAValueError, errorVariable = "Var (Name \"a\")"})
            let ast = run_parse "int tiny() { int * p; int a[5]; int x; a / p; }"
            run_semantics ast `shouldBe` Left (SemanticError {errorType = NotAValueError, errorVariable = "Var (Name \"a\")"})
            let ast = run_parse "int tiny() { int * p; int a[5]; int x; a / a; }"
            run_semantics ast `shouldBe` Left (SemanticError {errorType = NotAValueError, errorVariable = "Var (Name \"a\")"})
            let ast = run_parse "int tiny() { int * p; int a[5]; int x; a / a[5]; }"
            run_semantics ast `shouldBe` Left (SemanticError {errorType = NotAValueError, errorVariable = "Var (Name \"a\")"})
            let ast = run_parse "int tiny() { int * p; int a[5]; int x; a / x; }"
            run_semantics ast `shouldBe` Left (SemanticError {errorType = NotAValueError, errorVariable = "Var (Name \"a\")"})
            let ast = run_parse "int tiny() { int * p; int a[5]; int x; a / *p; }"
            run_semantics ast `shouldBe` Left (SemanticError {errorType = NotAValueError, errorVariable = "Var (Name \"a\")"})
            let ast = run_parse "int tiny() { int * p; int a[5]; int x; a + p; }"
            run_semantics ast `shouldBe` Left (SemanticError {errorType = NotAValueError, errorVariable = "Var (Name \"p\")"})
            let ast = run_parse "int tiny() { int * p; int a[5]; int x; a + a; }"
            run_semantics ast `shouldBe` Left (SemanticError {errorType = NotAValueError, errorVariable = "Var (Name \"a\")"})
            let ast = run_parse "int tiny() { int * p; int a[5]; int x; p + a[5]; }"
            run_semantics ast `shouldBe` Right ast
            let ast = run_parse "int tiny() { int * p; int a[5]; int x; p + x; }"
            run_semantics ast `shouldBe` Right ast
            let ast = run_parse "int tiny() { int * p; int a[5]; int x; p + *p; }"
            run_semantics ast `shouldBe` Right ast
            let ast = run_parse "int tiny() { int * p; int a[5]; int x; p - p; }"
            run_semantics ast `shouldBe` Right ast
            let ast = run_parse "int tiny() { int * p; int a[5]; int x; p - a; }"
            run_semantics ast `shouldBe` Right ast
            let ast = run_parse "int tiny() { int * p; int a[5]; int x; p - a[5]; }"
            run_semantics ast `shouldBe` Right ast
            let ast = run_parse "int tiny() { int * p; int a[5]; int x; p - x; }"
            run_semantics ast `shouldBe` Right ast
            let ast = run_parse "int tiny() { int * p; int a[5]; int x; p - *p; }"
            run_semantics ast `shouldBe` Right ast
            let ast = run_parse "int tiny() { int * p; int a[5]; int x; p * p; }"
            run_semantics ast `shouldBe` Left (SemanticError {errorType = NotAValueError, errorVariable = "Var (Name \"p\")"})
            let ast = run_parse "int tiny() { int * p; int a[5]; int x; p * a; }"
            run_semantics ast `shouldBe` Left (SemanticError {errorType = NotAValueError, errorVariable = "Var (Name \"p\")"})
            let ast = run_parse "int tiny() { int * p; int a[5]; int x; p * a[5]; }"
            run_semantics ast `shouldBe` Left (SemanticError {errorType = NotAValueError, errorVariable = "Var (Name \"p\")"})
            let ast = run_parse "int tiny() { int * p; int a[5]; int x; p * x; }"
            run_semantics ast `shouldBe` Left (SemanticError {errorType = NotAValueError, errorVariable = "Var (Name \"p\")"})
            let ast = run_parse "int tiny() { int * p; int a[5]; int x; p * *p; }"
            run_semantics ast `shouldBe` Left (SemanticError {errorType = NotAValueError, errorVariable = "Var (Name \"p\")"})
            let ast = run_parse "int tiny() { int * p; int a[5]; int x; p / p; }"
            run_semantics ast `shouldBe` Left (SemanticError {errorType = NotAValueError, errorVariable = "Var (Name \"p\")"})
            let ast = run_parse "int tiny() { int * p; int a[5]; int x; p / a; }"
            run_semantics ast `shouldBe` Left (SemanticError {errorType = NotAValueError, errorVariable = "Var (Name \"p\")"})
            let ast = run_parse "int tiny() { int * p; int a[5]; int x; p / a[5]; }"
            run_semantics ast `shouldBe` Left (SemanticError {errorType = NotAValueError, errorVariable = "Var (Name \"p\")"})
            let ast = run_parse "int tiny() { int * p; int a[5]; int x; p / x; }"
            run_semantics ast `shouldBe` Left (SemanticError {errorType = NotAValueError, errorVariable = "Var (Name \"p\")"})
            let ast = run_parse "int tiny() { int * p; int a[5]; int x; p / *p; }"
            run_semantics ast `shouldBe` Left (SemanticError {errorType = NotAValueError, errorVariable = "Var (Name \"p\")"})
        it "Checks that name subscriptions are used with an array" $ do
            let ast = run_parse "int a = 5; int tiny() { a[5] + 5; }"
            run_semantics ast `shouldBe` Left (SemanticError {errorType = NotAnArrayError, errorVariable = "NameSubscription \"a\" [Int 5]"})
        it "Checks that names are subscribed with scalar expressions" $ do
            let ast = run_parse "int a[5]; int b[6]; int tiny() { a[b]; }"
            run_semantics ast `shouldBe` Left (SemanticError {errorType = NotAValueError, errorVariable = "Var (Name \"b\")"})
            let ast = run_parse "int a[5]; int b[6]; int tiny() { a[b[3]]; }"
            run_semantics ast `shouldBe` Right ast
        it "Checks that assignments have the same scalarity" $ do
            let ast = run_parse "int a; int b; int tiny() { a = b; }"
            run_semantics ast `shouldBe` Right ast
            let ast = run_parse "int a; int b[5]; int tiny () { b = a; }"
            run_semantics ast `shouldBe` Left (SemanticError {errorType = CantAssignArrayError, errorVariable = "Name \"b\""})
        it "Checks that return statement have scalar expression" $ do
            let ast = run_parse "int tiny() { return 4; }"
            run_semantics ast `shouldBe` Right ast
            let ast = run_parse "int b[5]; int tiny() { return b; }"
            run_semantics ast `shouldBe` Left (SemanticError {errorType = NotAValueError, errorVariable = "Var (Name \"b\")"})
        it "Checks that if and while statements have scalar expressions" $ do
            let ast = run_parse "int tiny() { while (5) {} if (5) {} }"
            run_semantics ast `shouldBe` Right ast
            let ast = run_parse "int a[5]; int tiny() { while (a) {} }"
            run_semantics ast `shouldBe` Left (SemanticError {errorType = NotAValueError, errorVariable = "Var (Name \"a\")"})
            let ast = run_parse "int a[5]; int tiny() { if (a) {} }"
            run_semantics ast `shouldBe` Left (SemanticError {errorType = NotAValueError, errorVariable = "Var (Name \"a\")"})
            let ast = run_parse "int a[5]; int tiny() { if (a) {} else if (a) {} }"
            run_semantics ast `shouldBe` Left (SemanticError {errorType = NotAValueError, errorVariable = "Var (Name \"a\")"})
        it "Checks that one and only one entry point exists" $ do
            let ast = run_parse "int a;"
            run_semantics ast `shouldBe` Left (SemanticError {errorType = NoTinyFunctionError, errorVariable = ""})
            let ast = run_parse "int tiny() {} int tiny() {}"
            run_semantics ast `shouldBe` Left (SemanticError {errorType = NameExistsError, errorVariable = "tiny"})