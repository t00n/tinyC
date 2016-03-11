{
module Parser (parse, Declaration(..), Parameter(..), Statement(..), Expr(..), Type(..), BinaryOperator(..), UnaryOperator(..), Variable(..)) where
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
    name            { NAME $$ }
    qchar           { QCHAR $$ }
    qstring         { QSTRING $$ }

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
            | func_declaration                    { Just $1 }


var_declaration :: { Declaration }
var_declaration : type var '=' expr                { VarDeclaration $1 $2 (Just $4) }
                | type var                         { VarDeclaration $1 $2 Nothing }

func_declaration :: { Declaration }
func_declaration : type name '(' params ')' block { FuncDeclaration $1 (Variable $2) $4 $6 }


params :: { Parameters }
params : param            { [$1] }
                   | params ',' param { $1 ++ [$3] }
                   | {- empty -}                  { [] }

param :: { Parameter }
param : type var                      { Parameter $1 $2 }

var_declarations :: { [Declaration] }
var_declarations : var_declarations var_declaration ';' { $1 ++ [$2] }
                 | var_declarations ';'           { $1 }
                 | { [] }

block_or_not :: { Statement }
block_or_not : block { $1 }
             | statement ';' { $1 }

block :: { Statement }
block : '{' var_declarations statements '}'                  { Block $2 $3 }

statements :: { Statements }
statements : statements statement ';'                 { $1 ++ [$2] }
           | statements block_statement               { $1 ++ [$2] }
           | statements ';'       { $1 }
           | { [] }

statement :: { Statement }
statement : var '=' expr                          { Assignment $1 $3}
          | return expr                           { Return $2 }
          | write expr                            { Write $2 }
          | read var                              { Read $2 }
          | expr                                  { Expression $1 }

block_statement :: { Statement }
block_statement : if '(' expr ')' block_or_not             { If $3 $5 }
                | if '(' expr ')' block_or_not else block_or_not { IfElse $3 $5 $7 }
                | while '(' expr ')' block_or_not          { While $3 $5 }

args :: { [Expr] }
args : expr            { [$1] }
     | args ',' expr { $1 ++ [$3] }
     | {- empty -}                  { [] }



expr :: { Expr }
expr : number                                     { Int $1 }
     | qchar                                      { Char $1 }
     | var                                        { Var $1 }
     | expr '+' expr                              { BinOp $1 Plus $3 }
     | expr '-' expr                              { BinOp $1 Minus $3 }
     | expr '*' expr                              { BinOp $1 Times $3 }
     | expr '/' expr                              { BinOp $1 Divide $3 }
     | expr '==' expr                             { BinOp $1 Equal $3 }
     | expr '>' expr                              { BinOp $1 Greater $3 }
     | expr '<' expr                              { BinOp $1 Less $3 }
     | expr '!=' expr                             { BinOp $1 NotEqual $3 }
     | '(' expr ')'                               { $2 }
     | '-' expr %prec NEG                         { UnOp Neg $2 }
     | '!' expr                                   { UnOp Not $2 }
     | name '(' args ')'                          { Call (Variable $1) $3 }

type :: { Type }
type : int                                        { IntType }
     | char                                       { CharType }

var :: { Variable }
var : name  { Variable $1 }
    | name '[' expr ']' { Array $1 $3 }

{

parseError :: [Token] -> a
parseError _ = error "Parse error"

type Program = [Declaration]

data Declaration = VarDeclaration Type Variable (Maybe Expr)
                 | FuncDeclaration Type Variable Parameters Statement
    deriving (Eq, Show)

type Parameters = [Parameter]

data Parameter = Parameter Type Variable
    deriving (Eq, Show)

type Statements = [Statement]

data Statement = Assignment Variable Expr
               | If Expr Statement
               | IfElse Expr Statement Statement
               | While Expr Statement
               | Return Expr
               | Block [Declaration] Statements
               | Write Expr
               | Read Variable
               | Expression Expr
    deriving (Eq, Show)

data Type = IntType | CharType
    deriving (Eq, Show)

data BinaryOperator = Plus | Minus | Times | Divide | Equal | Greater | Less | NotEqual
    deriving (Eq, Show)

data UnaryOperator = Neg | Not
    deriving (Eq, Show)

data Expr = Int Int
          | Char Char
          | Var Variable
          | BinOp Expr BinaryOperator Expr
          | UnOp UnaryOperator Expr
          | Call Variable [Expr]
    deriving (Eq, Show)

data Variable = Variable String
              | Array String Expr
    deriving (Eq, Show)
}