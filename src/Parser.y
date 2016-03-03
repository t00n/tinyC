{
module Parser (parse, Statement(..), Expr(..), Type(..), BinaryOperator(..), UnaryOperator(..)) where
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
%left '<' '>'
%left '+' '-'
%left '*' '/'
%right '!' NEG
%left '[' ']' '(' ')' '{' '}'
%%

Statements :: { Statements }
Statements : Statement ';'                        { [$1] }
           | Statements Statement ';'             { $1 ++ [$2] }
           | {- empty -} ';'                      { [] }
           | Statements ';'                       { $1 }
           | Block                                { [$1] }

Statement :: { Statement }
Statement : int var '=' Expr                      { Declaration IntType $2 (Just $4) }
          | int var                               { Declaration IntType $2 Nothing }
          | char var '=' Expr                     { Declaration CharType $2 (Just $4) }
          | char var                              { Declaration CharType $2 Nothing}
          | var '=' Expr                          { Assignment $1 $3}
          | if '(' Expr ')' Statement             { If $3 $5 }
          | while '(' Expr ')' Statement          { While $3 $5 }

Block :: { Statement }
Block : if '(' Expr ')' '{' Statements '}'        { IfBlock $3 $6 }
      | while '(' Expr ')' '{' Statements '}'     { WhileBlock $3 $6 }


Expr :: { Expr }
Expr : number                                     { Int $1 }
     | qchar                                      { Char $1 }
     | var                                        { Var $1 }
     | Expr '+' Expr                              { BinOp $1 Plus $3 }
     | Expr '-' Expr                              { BinOp $1 Minus $3 }
     | Expr '*' Expr                              { BinOp $1 Times $3 }
     | Expr '/' Expr                              { BinOp $1 Divide $3 }
     | Expr '==' Expr                             { BinOp $1 Equal $3 }
     | Expr '>' Expr                              { BinOp $1 Greater $3 }
     | Expr '<' Expr                              { BinOp $1 Less $3 }
     | Expr '!=' Expr                             { BinOp $1 NotEqual $3 }
     | '-' Expr %prec NEG                         { UnOp Neg $2 }
     | '!' Expr                                   { UnOp Not $2 }

{

parseError :: [Token] -> a
parseError _ = error "Parse error"

type Statements = [Statement]

data Statement = Declaration Type String (Maybe Expr)
               | Assignment String Expr
               | If Expr Statement
               | IfBlock Expr Statements
               | While Expr Statement
               | WhileBlock Expr Statements
    deriving (Eq, Show)

data Type = IntType | CharType
    deriving (Eq, Show)

data BinaryOperator = Plus | Minus | Times | Divide | Equal | Greater | Less | NotEqual
    deriving (Eq, Show)

data UnaryOperator = Neg | Not
    deriving (Eq, Show)

data Expr = Int Int
          | Char Char
          | Var String
          | BinOp Expr BinaryOperator Expr
          | UnOp UnaryOperator Expr
    deriving (Eq, Show)
}