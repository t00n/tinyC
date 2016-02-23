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

%%

Statements : Statement                { $1 }
           | Statements Statement     { $1 ++ $2 }
           | {- empty -}              { [] }

Statement : Statement1 ';'            { [$1] }
          | {- empty -} ';'           { [] }

Statement1 : int var '=' number       { Declaration $1 $2 $4 }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

type Statements = [Statement]

data Statement = Declaration Token String Int
    deriving (Eq, Show)
}