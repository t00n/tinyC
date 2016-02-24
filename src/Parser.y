{
module Parser (parse, Statement(..), Expr(..)) where
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

Statement : Statement1 ';'            { [$1] }
          | {- empty -} ';'           { [] }

Statement1 : int var '=' Expr         { Declaration $1 $2 (Just $4) }
           | int var                  { Declaration $1 $2 Nothing }

Expr : number                         { Lit $1 }
     | Expr '+' Expr                  { Operator $1 $2 $3 }
     | Expr '-' Expr                  { Operator $1 $2 $3 }
     | Expr '*' Expr                  { Operator $1 $2 $3 }
     | Expr '/' Expr                  { Operator $1 $2 $3 }
{
parseError :: [Token] -> a
parseError _ = error "Parse error"

type Statements = [Statement]

data Statement = Declaration Token String (Maybe (Expr Int))
    deriving (Eq, Show)

data Expr a = Lit a
            | Operator (Expr a) Token (Expr a)
    deriving (Eq, Show)
}