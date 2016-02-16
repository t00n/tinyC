{
    module Scanner where
}

%wrapper "basic"
$digit = 0-9
$alpha = [a-zA-Z]
$word = [$alpha$digit\_]

tokens :- 
    $white+         ; -- spaces etc
    \/\/.*          ; -- comments
    int             { \s -> INT }
    if              { \s -> IF }
    else            { \s -> ELSE }
    !=              { \s -> NEQUAL }
    return          { \s -> RETURN }
    \(              { \s -> LPAR }
    \)              { \s -> RPAR }
    \{              { \s -> LBRACE }
    \}              { \s -> RBRACE }
    \[              { \s -> LBRACK }
    \]              { \s -> RBRACK }
    =               { \s -> ASSIGN }
    \;              { \s -> SEMICOLON }
    \,              { \s -> COMMA }
    \+              { \s -> PLUS }
    \-              { \s -> MINUS }
    \*              { \s -> TIMES }
    \/              { \s -> DIVIDE }
    ==              { \s -> EQUAL }
    char            { \s -> CHAR }
    write           { \s -> WRITE }
    read            { \s -> READ }
    >               { \s -> GREATER }
    \<              { \s -> LESS }
    !               { \s -> NOT }
    length          { \s -> LENGTH }
    while           { \s -> WHILE }
    $digit+         { \s -> NUMBER (read s) }
    $alpha[$word]*  { \s -> NAME s }
    \'.\'           { \s -> QCHAR (s !! 1) }
    \".*"           { \s -> QString $ take ((length s) - 2) (drop 1 s) }

{
data Token = INT | IF | ELSE | NEQUAL
           | RETURN | LPAR | RPAR | LBRACE
           | RBRACE | LBRACK | RBRACK | ASSIGN
           | SEMICOLON | COMMA | PLUS | MINUS
           | TIMES | DIVIDE | EQUAL | CHAR
           | WRITE | READ | GREATER | LESS
           | NOT | LENGTH | WHILE
           | NAME String | NUMBER Int | QCHAR Char | QString String
           deriving (Eq, Show)
}