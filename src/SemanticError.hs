module SemanticError where
    
data ErrorType = NotDeclaredError 
               | NotAFunctionError 
               | NameExistsError 
               | NotAnArrayError 
               | NotAScalarError
               | NotSameScalarityError
               | NameExistsWarning
               | NotConstantSizeArrayError
               | NoTinyFunctionError
               | SeveralTinyFunctionError
    deriving (Eq, Show)

data SemanticError = SemanticError {
    errorType :: ErrorType,
    errorVariable :: String
} deriving (Eq, Show)