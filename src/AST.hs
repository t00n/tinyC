module AST where

type Program = [Declaration]

data Declaration = VarDeclaration Type Name (Maybe Expression)
                 | FuncDeclaration Type Name [Parameter] Statement
    deriving (Eq, Show)

data Parameter = Parameter Type Name
    deriving (Eq, Show)

data Statement = Assignment Name Expression
               | If Expression Statement
               | IfElse Expression Statement Statement
               | While Expression Statement
               | Return Expression
               | Block [Declaration] [Statement]
               | Write Expression
               | Read Name
               | Expr Expression
    deriving (Eq, Show)

data Type = IntType | CharType | PtrType
    deriving (Eq, Show, Ord)

data BinaryOperator = Plus | Minus | Times | Divide | Equal | Greater | Less | NotEqual
    deriving (Eq, Show)

data UnaryOperator = Neg | Not
    deriving (Eq, Show)

data Expression = Int Int
                | Char Char
                | BinOp Expression BinaryOperator Expression
                | UnOp UnaryOperator Expression
                | Call Name [Expression]
                | Length Name
                | Var Name
                | Address Name
    deriving (Eq, Show)

data Name = Name String
          | NameSubscription String [Expression]
          | NamePointer String
    deriving (Eq, Show)

nameToString :: Name -> String
nameToString (Name s) = s
nameToString (NameSubscription s _) = s
nameToString (NamePointer s) = s