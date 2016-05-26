module SemanticError where
    
data ErrorType = NotDeclaredError 
               | NotAFunctionError 
               | NameExistsError 
               | NotAnArrayError 
               | NotAScalarError
               | NotSameScalarityError
               | NameExistsWarning
               | NotConstantError
               | NoTinyFunctionError
    deriving (Eq, Show)

data SemanticError = SemanticError {
    errorType :: ErrorType,
    errorVariable :: String
} deriving (Eq, Show)