{
module Parser (parse, Statement(..)) where
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

%left '+' '-'
%left '*' '/'
%%

Statements : Statement                { $1 }
           | Statements Statement     { $1 ++ $2 }

Statement : Statement1 ';'            { [$1] }
          | {- empty -} ';'           { [] }

Statement1 : int var '=' IntExpr      { Declaration $1 $2 (Just $4) }
           | int var                  { Declaration $1 $2 Nothing }

IntExpr : number                      { $1 }
        | IntExpr '+' IntExpr         { $1 + $3 }
        | IntExpr '-' IntExpr         { $1 - $3 }
        | IntExpr '*' IntExpr         { $1 * $3 }
        | IntExpr '/' IntExpr         { $1 `div` $3 }
{
parseError :: [Token] -> a
parseError _ = error "Parse error"

type Statements = [Statement]

data Statement = Declaration Token String (Maybe Int)
    deriving (Eq, Show)

data IntExpr = Int
             | IntExpr Token IntExpr
}