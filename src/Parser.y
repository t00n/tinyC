{
module Parser (parse, Statement(..), Expr(..), Type(..), BinaryOperator(..)) where
import Scanner (Token(..))
}

%name parse
%tokentype { Token }
%error { parseError }

%token
    int             { INT }
    if              { IF }
    else            { ELSE }
    '!='            { NEQUAL }
    return          { RETURN }
    '('             { LPAR }
    ')'             { RPAR }
    '{'             { LBRACE }
    '}'             { RBRACE }
    '['             { LBRACK }
    ']'             { RBRACK }
    '='             { ASSIGN }
    ';'             { SEMICOLON }
    ','             { COMMA }
    '+'             { PLUS }
    '-'             { MINUS }
    '*'             { TIMES }
    '/'             { DIVIDE }
    '=='            { EQUAL }
    char            { CHAR }
    write           { WRITE }
    read            { READ }
    '>'             { GREATER }
    '<'             { LESS }
    '!'             { NOT }
    length          { LENGTH }
    while           { WHILE }
    number          { NUMBER $$ }
    var             { NAME $$ }
    qchar           { QCHAR $$ }
    qstring         { QString $$ }

%left ','
%right '='
%left '==' '!='
%left '+' '-'
%left '*' '/'
%right '!'
%left '[' ']' '(' ')'
%%

Statements : Statement                { $1 }
           | Statements Statement     { $1 ++ $2 }

Statement : Instruction ';'           { [$1] }
          | {- empty -} ';'           { [] }

Instruction : int var '=' Expr        { Declaration IntType $2 (Just $4) }
           | int var                  { Declaration IntType $2 Nothing }
           | char var '=' Expr        { Declaration CharType $2 (Just $4) }
           | char var                 { Declaration CharType $2 Nothing}
           | var '=' Expr             { Assignment $1 $3}
           | if '(' Expr ')' Instruction{ If $3 $5 }
           {-| if '(' Expr ')'
                '{' Statements '}'    { IfBlock $3 $6 }-}

Expr : number                         { Int $1 }
     | qchar                          { Char $1 }
     | var                            { Var $1 }
     | Expr '+' Expr                  { Operator $1 Plus $3 }
     | Expr '-' Expr                  { Operator $1 Minus $3 }
     | Expr '*' Expr                  { Operator $1 Times $3 }
     | Expr '/' Expr                  { Operator $1 Divide $3 }
     | Expr '==' Expr                 { Operator $1 Equal $3 }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

type Statements = [Statement]

data Statement = Declaration Type String (Maybe Expr)
               | Assignment String Expr
               | If Expr Statement
               -- | IfBlock Expr Statements
    deriving (Eq, Show)

data Type = IntType | CharType
    deriving (Eq, Show)

data BinaryOperator = Plus | Minus | Times | Divide | Equal
    deriving (Eq, Show)

data Expr = Int Int
            | Char Char
            | Var String
            | Operator Expr BinaryOperator Expr
    deriving (Eq, Show)
}