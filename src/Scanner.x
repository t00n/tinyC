{
    module Scanner where
}

%wrapper "basic"
$digit = 0-9
-- digits
$alpha = [a-zA-Z]
-- alphabetic characters

tokens :- 
    $white+     ;
    $digit+     { \s -> INT (read s) }

{
data Token = INT Int | IF | ELSE | NEQUAL
           | RETURN | LPAR | RPAR | LBRACE
           | RBRACE | LBRACK | RBRACK | ASSIGN
           | SEMICOLON | COMMA | PLUS | MINUS
           | TIMES | DIVIDE | EQUAL | CHAR
           | WRITE | READ | GREATER | LESS
           | NOT | LENGTH | WHILE
           | NAME | NUMBER | QCHAR
           deriving (Eq, Show)
}