{
module Parser (parse, Declaration(..), Statement(..), Expr(..), Type(..), BinaryOperator(..), UnaryOperator(..)) where
import Scanner (Token(..))
import Data.Maybe
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

program :: { Program }
program : declaration                             { if isJust $1 then [fromJust $1] else [] }
        | program declaration                     { $1 ++ if isJust $2 then [fromJust $2] else [] }

declaration :: { Maybe Declaration }
declaration : var_declaration ';'                 { Just $1 }
            | {-empty -} ';'                      { Nothing }


var_declaration :: { Declaration }
var_declaration : int var '=' Expr                { VarDeclaration IntType $2 (Just $4) }
                | int var                         { VarDeclaration IntType $2 Nothing }
                | char var '=' Expr               { VarDeclaration CharType $2 (Just $4) }
                | char var                        { VarDeclaration CharType $2 Nothing}
          {-| var '=' Expr                          { Assignment $1 $3}
          | if '(' Expr ')' Statement             { If $3 $5 }
          | while '(' Expr ')' Statement          { While $3 $5 }-}

{-Block :: { Statement }
Block : if '(' Expr ')' '{' Statements '}'        { IfBlock $3 $6 }
      | while '(' Expr ')' '{' Statements '}'     { WhileBlock $3 $6 }-}


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

type Program = [Declaration]

data Declaration = VarDeclaration Type String (Maybe Expr)
    deriving (Eq, Show)

data Statement = Assignment String Expr
               | If Expr Statement
               | IfBlock Expr Statement
               | While Expr Statement
               | WhileBlock Expr Statement
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