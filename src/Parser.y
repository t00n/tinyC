{
module Parser (parse, Program, Declaration(..), Parameter(..), Statement(..), Expression(..), Type(..), BinaryOperator(..), UnaryOperator(..), Name(..), nameString) where
import Scanner
import Data.Maybe
import Text.Printf (printf)
}

%name parse
%tokentype { TokenWrapper }
%error { parseError }

%token
    int             { TokenWrapper (INT) (_, _, _) }
    if              { TokenWrapper (IF) (_, _, _) }
    else            { TokenWrapper (ELSE) (_, _, _) }
    '!='            { TokenWrapper (NEQUAL) (_, _, _) }
    return          { TokenWrapper (RETURN) (_, _, _) }
    '('             { TokenWrapper (LPAR) (_, _, _) }
    ')'             { TokenWrapper (RPAR) (_, _, _) }
    '{'             { TokenWrapper (LBRACE) (_, _, _) }
    '}'             { TokenWrapper (RBRACE) (_, _, _) }
    '['             { TokenWrapper (LBRACK) (_, _, _) }
    ']'             { TokenWrapper (RBRACK) (_, _, _) }
    '='             { TokenWrapper (ASSIGN) (_, _, _) }
    ';'             { TokenWrapper (SEMICOLON) (_, _, _) }
    ','             { TokenWrapper (COMMA) (_, _, _) }
    '+'             { TokenWrapper (PLUS) (_, _, _) }
    '-'             { TokenWrapper (MINUS) (_, _, _) }
    '*'             { TokenWrapper (TIMES) (_, _, _) }
    '/'             { TokenWrapper (DIVIDE) (_, _, _) }
    '=='            { TokenWrapper (EQUAL) (_, _, _) }
    char            { TokenWrapper (CHAR) (_, _, _) }
    write           { TokenWrapper (WRITE) (_, _, _) }
    read            { TokenWrapper (READ) (_, _, _) }
    '>'             { TokenWrapper (GREATER) (_, _, _) }
    '<'             { TokenWrapper (LESS) (_, _, _) }
    '!'             { TokenWrapper (NOT) (_, _, _) }
    length          { TokenWrapper (LENGTH) (_, _, _) }
    while           { TokenWrapper (WHILE) (_, _, _) }
    number          { TokenWrapper (NUMBER $$) (_, _, _) }
    name            { TokenWrapper (NAME $$) (_, _, _) }
    qchar           { TokenWrapper (QCHAR $$) (_, _, _) }
    qstring         { TokenWrapper (QSTRING $$) (_, _, _) }

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
var_declaration : type var '=' expr               { VarDeclaration $1 $2 (Just $4) }
                | type var                        { VarDeclaration $1 $2 Nothing }

func_declaration :: { Declaration }
func_declaration : type name '(' params ')' block { FuncDeclaration $1 (Name $2) (reverse $4) $6 }


params ::{ [Parameter] }
params : param                                    { [$1] }
       | params ',' param                         { $3 : $1 }
       | {- empty -}                              { [] }

param :: { Parameter }
param : type var                                  { Parameter $1 $2 }

var_declarations :: { [Declaration] }
var_declarations : var_declarations 
                       var_declaration ';'        { $2 : $1 }
                 | var_declarations ';'           { $1 }
                 | {- empty -}                    { [] }

block_or_not :: { Statement }
block_or_not : block                              { $1 }
             | statement ';'                      { $1 }
             | block_statement                    { $1 }

block :: { Statement }
block : '{' var_declarations statements '}'       { Block (reverse $2) (reverse $3) }

statements :: { [Statement] }
statements : statements statement ';'             { $2 : $1 }
           | statements block_statement           { $2 : $1 }
           | statements ';'                       { $1 }
           | {- empty -}                          { [] }

statement :: { Statement }
statement : var '=' expr                          { Assignment $1 $3}
          | return expr                           { Return $2 }
          | write expr                            { Write $2 }
          | read var                              { Read $2 }
          | expr                                  { Expr $1 }

block_statement :: { Statement }
block_statement : if '(' expr ')' block_or_not    { If $3 $5 }
                | if '(' expr ')' block_or_not 
                  else block_or_not               { IfElse $3 $5 $7 }
                | while '(' expr ')' block_or_not { While $3 $5 }

args :: { [Expression] }
args : expr                                       { [$1] }
     | args ',' expr                              { $3 : $1 }
     | {- empty -}                                { [] }



expr :: { Expression }
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
     | name '(' args ')'                          { Call (Name $1) (reverse $3) }
     | length var                                 { Length $2 }

type :: { Type }
type : int                                        { IntType }
     | char                                       { CharType }

var :: { Name }
var : name                                        { Name $1 }
    | name '[' expr ']'                           { NameSubscription $1 $3 }

{

parseError :: [TokenWrapper] -> a
parseError [] = error ""
parseError ((TokenWrapper token (o, l, c)):_) = error $ printf "Parse error at line %d column %d : \"%s\"" l c (show token)

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

data Type = IntType | CharType
    deriving (Eq, Show)

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
    deriving (Eq, Show)

data Name = Name String
          | NameSubscription String Expression
    deriving (Eq, Show)

nameString :: Name -> String
nameString (Name s) = s
nameString (NameSubscription s _) = s
}