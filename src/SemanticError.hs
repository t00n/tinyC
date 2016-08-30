module SemanticError where
    
data ErrorType = NotDeclaredError 
               | NameExistsError 
               | NameExistsWarning
               | NotAFunctionError 
               | NotAPointerError 
               | NotAnArrayError
               | NotAValueError
               | NotAConstantError
               | NoTinyFunctionError
               | CantAssignArrayError
               | TooMuchSubscriptionError
    deriving (Eq, Show)

data SemanticError = SemanticError {
    errorType :: ErrorType,
    errorVariable :: String
} deriving (Eq, Show)